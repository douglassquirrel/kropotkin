#!/usr/bin/python
from base64 import b64decode
from contextlib import closing
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os import access, environ, listdir, path, X_OK
from os.path import isdir, join
from socket import getfqdn
from StringIO import StringIO
from subprocess import Popen
from sys import stderr
from tarfile import open as taropen
from tempfile import mkdtemp

def unpack(name, tar_data):
    directory = mkdtemp(prefix=name)
    with closing(StringIO(b64decode(tar_data))) as buffer:
        with taropen(mode='r', fileobj=buffer) as tar:
            tar.extractall(path=directory)
    return directory

def inherit(to, from_, keys):
    for key in keys:
        if key not in to and key in from_:
            to[key] = from_[key]

def deploy(name, directory, env={}):
    executable = get_unique_executable(directory)
    if not executable:
        stderr.write("No unique executable in %s for %s\n" % (directory, name))
        return False

    env = env.copy()
    inherit(env, environ, ['KROPOTKIN_QUEUE', 'KROPOTKIN_URL',
                           'TEMP', 'TMP', 'TMPDIR'])
    process = Popen(executable, cwd=directory, env=env)
    content = {'name': name, 'location': getfqdn(),
               'identifier': process.pid}
    if 'KROPOTKIN_URL' in environ:
        if not store_fact('kropotkin', 'component_deployed', content):
            stderr.write('Cannot store component_deployed fact for %s\n' %name)
    return process.pid

def get_unique_executable(directory):
    nodes = listdir(directory)
    executables = [f for f in nodes if is_executable_file(join(directory, f))]
    return executables[0] if len(executables) == 1 else None

def is_executable_file(f):
    return (not isdir(f)) and access(f, X_OK)

if __name__=="__main__":
    while True:
        component_fact \
            = get_oldest_fact_and_stamp('kropotkin',
                                        'component',
                                        {'content_type': 'component-tar'},
                                        'deployer')
        if component_fact:
            name = component_fact['name']
            directory = unpack(name, component_fact['bytes'])
            deploy(name, directory)
