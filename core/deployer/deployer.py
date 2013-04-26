#!/usr/bin/python
from base64 import b64decode
from contextlib import closing
from kropotkin import get_oldest_fact_and_stamp
from os import access, environ, listdir, path, X_OK
from os.path import isdir, join
from subprocess import Popen
from StringIO import StringIO
import tarfile
from tempfile import mkdtemp

def unpack(name, tar_data):
    directory = mkdtemp(prefix=name)
    with closing(StringIO(b64decode(tar_data))) as buffer:
        with tarfile.open(mode='r', fileobj=buffer) as tar:
            tar.extractall(path=directory)
    return directory

def deploy(directory, kropotkin_url):
    executable = get_unique_executable(directory)
    if executable:
        Popen(executable, cwd=directory, env={'KROPOTKIN_URL': kropotkin_url})
    else:
        print "Cannot locate unique executable in %s" % directory

def get_unique_executable(directory):
    nodes = listdir(directory)
    executables = [f for f in nodes if is_executable_file(join(directory, f))]
    return executables[0] if len(executables) == 1 else None

def is_executable_file(f):
    return (not isdir(f)) and access(f, X_OK)

if __name__=="__main__":
    KROPOTKIN_URL = environ['KROPOTKIN_URL']
    while True:
        component_fact = get_oldest_fact_and_stamp(KROPOTKIN_URL,
                                                   'component', \
                                                   {'language': 'python'}, \
                                                   'deployer.1414')
        if component_fact:
            directory = unpack(component_fact['name'], component_fact['tar'])
            deploy(directory, KROPOTKIN_URL)
