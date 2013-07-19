#!/usr/bin/python
from base64 import b64encode
from contextlib import closing
from kropotkin import get_next_fact, store_fact, subscribe
from os import access, listdir, path, X_OK
from os.path import isdir, join, basename
from StringIO import StringIO
from tarfile import open as taropen

def publish(location):
    if isdir(location):
        files = [join(location, f) for f in listdir(location) if not isdir(f)]
        name = basename(location)
        language = determine_language(files)
        bytes = archive(files)
        content_type = 'component-tar'
    else:
        name = basename(location)
        language = language_type(location)
        with open(location) as f:
            bytes = b64encode(f.read())
        content_type = language
    content = {'name': name, 'language': language,
               'content_type': content_type, 'bytes': bytes}
    if not store_fact('kropotkin', 'component', content):
        raise Exception("Cannot store component fact")

def determine_language(files):
    executable = get_unique_executable(files)
    return language_type(executable) if executable else None

def get_unique_executable(files):
    executables = [f for f in files if access(f, X_OK)]
    return executables[0] if len(executables) == 1 else None

HASHBANG_TYPE = {'#!/usr/bin/python': 'python'}
EXTENSION_TYPE = {'py':   'python',
                  'js':   'javascript',
                  'html': 'html'}
def language_type(filename):
    if access(filename, X_OK):
        with open(filename, 'r') as e:
            hashbang = e.readline()[:-1]
            return HASHBANG_TYPE.get(hashbang)
    else:
        extension = filename.split('.')[-1]
        return EXTENSION_TYPE[extension]

def archive(files):
    with closing(StringIO()) as buffer:
        with taropen(mode='w', fileobj=buffer) as tar:
            for f in files:
                tar.add(f, arcname=basename(f))
        return b64encode(buffer.getvalue())

if __name__=="__main__":
    subscribe('kropotkin', 'fact', 'component_available')
    while True:
        fact = get_next_fact('kropotkin', 'component_available')
        publish(fact['location'])
