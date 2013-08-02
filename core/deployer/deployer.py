#!/usr/bin/python
from base64 import b64decode
from contextlib import closing
from kropotkin import get_next_fact, store_fact, subscribe
from os import access, environ, listdir, path, X_OK
from os.path import abspath, isdir, join
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

def deploy(name, directory, store_deployed_fact=True, temp_output=False):
    directory = abspath(directory)
    executable = get_unique_executable(directory)
    if not executable:
        stderr.write("No unique executable in %s for %s\n" % (directory, name))
        return False

    if temp_output is True:
        output_directory = mkdtemp(prefix=name)
    else:
        output_directory = directory

    stdout_file = open_output_file(output_directory, name, 'stdout')
    stderr_file = open_output_file(output_directory, name, 'stderr')

    process = Popen(executable, cwd=directory,
                    stdout=stdout_file, stderr=stderr_file)

    content = {'name': name,
               'location': getfqdn(),
               'identifier': process.pid,
               'stdout_file': stdout_file.name,
               'stderr_file': stderr_file.name}

    if store_deployed_fact is True:
        if not store_fact('kropotkin', 'component_deployed', content):
            stderr.write('Cannot store component_deployed fact for %s\n' %name)
    return content

def get_unique_executable(directory):
    nodes = listdir(directory)
    executables = [f for f in nodes if is_executable_file(join(directory, f))]
    return join(directory, executables[0]) if len(executables) == 1 else None

def is_executable_file(f):
    return (not isdir(f)) and access(f, X_OK)

def open_output_file(directory, process_name, file_name):
    basename = '.'.join([process_name, file_name, 'log'])
    filename = join(directory, basename)
    return open(filename, 'w')

if __name__=="__main__":
    subscribe('kropotkin', 'fact', 'component')
    while True:
        component_fact = get_next_fact('kropotkin', 'component')
        if component_fact['content_type'] == 'component-tar':
            name = component_fact['name']
            directory = unpack(name, component_fact['bytes'])
            deploy(name, directory)
