#!/usr/bin/python
from base64 import b64encode
from contextlib import closing
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os import access, environ, listdir, path, X_OK
from os.path import isdir, join, basename
from StringIO import StringIO
import tarfile

def publish(directory, kropotkin_url):
    nodes = set([join(directory, f) for f in listdir(directory)])
    dirs = set([f for f in nodes if isdir(f)])
    files = nodes - dirs

    for d in dirs:
        publish(d, kropotkin_url)

    name = basename(directory)
    language = determine_language(files)
    tar = tar_files(files)
    content = {'name': name, 'language': language, 'tar': tar}
    store_fact(kropotkin_url, 'component', content)

def tar_files(files):
    with closing(StringIO()) as buffer:
        with tarfile.open(mode='w', fileobj=buffer) as tar:
            for f in files:
                tar.add(f, arcname=basename(f))
        return b64encode(buffer.getvalue())

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

if __name__=="__main__":
    KROPOTKIN_URL = environ['KROPOTKIN_URL']
    while True:
        fact = get_oldest_fact_and_stamp(KROPOTKIN_URL, \
                                         'component_available',
                                         {}, \
                                         'publisher.2718')
        if fact:
            publish(fact['directory'], KROPOTKIN_URL)
