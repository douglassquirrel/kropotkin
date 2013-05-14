#!/usr/bin/python
from httplib2 import Http
from inspect import getsource
from os import environ, listdir
from os.path import abspath, isdir, join
from sys import exit
from time import time
try:
    import kropotkin
    with open('libraries/python/kropotkin.py', 'r') as f:
        expected_lib_src = f.read()
    actual_lib_src = getsource(kropotkin)
    kropotkin_imported = actual_lib_src == expected_lib_src
except ImportError:
    kropotkin_imported = False
if not kropotkin_imported:
    print "Kropotkin module not installed or out of date."
    print "See libraries/python/README.txt for installation steps."
    exit(1)

from core.deployer.deployer import deploy
from core.factspace.factspace import create_factspace

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

def dirs_in(parent):
    parent = abspath(parent)
    return [d for d in listdir(parent) if isdir(join(parent, d))]

env = {'KROPOTKIN_URL': KROPOTKIN_URL, 'KROPOTKIN_DIR': KROPOTKIN_DIR}
deploy('http', 'http', env)
if not wait_for_http(10):
    print "Http not starting"
    exit(1)

for c in dirs_in('core'):
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
              'translation': 'Factspace %(name)s created in %(directory)s'},
            {'type': 'library_available',
             'keys': ['directory'],
             'translation': 'Library available in %(directory)s'}]
for e in elements:
    if not kropotkin.store_fact('kropotkin', 'constitution_element', e):
        print 'Could not store %s' % e
        exit(1)

for c in dirs_in('components'):
    component_location = abspath(join('components', c))
    if not kropotkin.store_fact('kropotkin', 'component_available',
                                {'directory': component_location}):
        print 'Could not store component_available for %s' % c
        exit(1)

for lib in dirs_in('libraries'):
    library_location = abspath(join('libraries', lib))
    if not kropotkin.store_fact('kropotkin', 'library_available',
                                {'directory': library_location}):
        print 'Could not store library_available for %s' % lib
        exit(1)
