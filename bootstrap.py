#!/usr/bin/python
from core.deployer.deployer import deploy
from core.factspace.factspace import create_factspace
from httplib2 import Http
from kropotkin import store_fact
from os import environ, listdir, walk
from os.path import abspath, isdir, join
from sys import exit
from tempfile import mkdtemp
from time import time

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
KROPOTKIN_DIR=create_factspace('kropotkin')

def wait_for_http(timeout):
    finish = now() + timeout
    http = Http()
    while True:
        try:
            response, content = http.request(KROPOTKIN_URL, "GET")
            return True
        except IOError:
            pass
        if now() > finish:
            return False

def now():
    return int(round(time()))

env = {'KROPOTKIN_URL': KROPOTKIN_URL, 'KROPOTKIN_DIR': KROPOTKIN_DIR}
deploy('http', 'http', env)
if not wait_for_http(10):
    print "Http not starting"
    exit(1)

core_components = [d for d in listdir('core') if isdir(join('core', d))]
for c in core_components:
    deploy(c, join('core', c), env)

environ['KROPOTKIN_URL'] = KROPOTKIN_URL
elements = [{'type': 'component_available',
              'keys': ['directory'],
              'translation': 'Component available in %(directory)s'},
            {'type': 'component',
             'keys': ['name', 'tar', 'language'],
             'translation': 'Component %(name)s, language %(language)s'},
            {'type': 'factspace_wanted',
              'keys': ['name'],
              'translation': 'Factspace %(name)s requested'},
            {'type': 'factspace',
              'keys': ['name', 'directory'],
              'translation': 'Factspace %(name)s created in %(directory)s'}]
for e in elements:
    if not store_fact('kropotkin', 'constitution_element', e):
        print 'Could not store %s' % e
        exit 1

for root, dirs, files in walk('components'):
    for d in dirs:
        content = {'directory': abspath(join(root, d))}
        if not store_fact('kropotkin', 'component_available', content):
            print 'Could not store %s' % content
            exit 1
