#!/usr/bin/python
from base64 import b64encode
from contextlib import closing
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os import access, listdir, path, X_OK
from os.path import isdir, join, basename
from StringIO import StringIO
from tarfile import open as taropen

def publish(directory):
    files = [join(directory, f) for f in listdir(directory) if not isdir(f)]
    name = basename(directory)
    language = determine_language(files)
    tar = archive(files)
    content = {'name': name, 'language': language, 'tar': tar}
    store_fact('kropotkin', 'component', content)

def determine_language(files):
    executable = get_unique_executable(files)
    return language_type(executable) if executable else None

def get_unique_executable(files):
    executables = [f for f in files if access(f, X_OK)]
    return executables[0] if len(executables) == 1 else None

TYPE = {'#!/usr/bin/python': 'python'}
def language_type(executable):
    with open(executable, 'r') as e:
        hashbang = e.readline()[:-1]
        return TYPE.get(hashbang)

def archive(files):
    with closing(StringIO()) as buffer:
        with taropen(mode='w', fileobj=buffer) as tar:
            for f in files:
                tar.add(f, arcname=basename(f))
        return b64encode(buffer.getvalue())

if __name__=="__main__":
    while True:
        fact = get_oldest_fact_and_stamp('kropotkin', \
                                         'component_available', \
                                         {}, \
                                         'publisher.2718')
        if fact:
            publish(fact['directory'])
