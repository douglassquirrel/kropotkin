#!/usr/bin/python
from base64 import b64encode
from contextlib import closing
from glob import glob
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os import access, environ, path, X_OK
from os.path import isdir, join, basename
import tarfile
from StringIO import StringIO

def deploy(directory, kropotkin_url):
    nodes = set(glob(join(directory, "*")))
    dirs = set([f for f in nodes if isdir(f)])
    files = nodes - dirs

    for d in dirs:
        deploy(join(directory, d), kropotkin_url)

    name = basename(directory)
    language = determine_language(directory, files)
    tar = tar_files(directory, files)
    content = {'name': name, 'language': language, 'tar': tar}
    store_fact(kropotkin_url, 'component', content)

def tar_files(directory, files):
    with closing(StringIO()) as buffer:
        with tarfile.open(mode='w', fileobj=buffer) as tar:
            for f in files:
                tar.add(join(directory, f), arcname=f)
        return b64encode(buffer.getvalue())

def determine_language(directory, files):
    executable = get_unique_executable(directory, files)
    return language_type(executable) if executable else None

def get_unique_executable(directory, files):
    executables = [f for f in files if access(join(directory, f), X_OK)]
    return executables[0] if len(executables) == 1 else None

TYPE = {'#!/usr/bin/python': 'python'}
def language_type(executable):
    with open(executable, 'r') as e:
        hashbang = e.readline()[:-1]
        return TYPE.get(hashbang)

if __name__=="__main__":
    KROPOTKIN_URL = environ['KROPOTKIN_URL']
    while True:
        deploy_fact = get_oldest_fact_and_stamp(KROPOTKIN_URL, 'deploy', {}, \
                                                'deploy2.2718')
        if deploy_fact:
            deploy(deploy_fact['directory'], KROPOTKIN_URL)
