#!/usr/bin/python
from sys import exit, stderr, version, version_info

def fail_and_exit(message):
    stderr.write(message + '\n')
    exit(1)

major_version = version_info[0]
minor_version = version_info[1]
if (major_version != 2 or minor_version < 7):
    fail_and_exit('Kropotkin works with Python 2, version 2.7 or greater.\n' \
                + 'Your version does not match - it is:\n' + version)

from contextlib import closing
from inspect import getsource
from json import dumps
from os import environ, listdir
from os.path import abspath, isdir, join
from socket import gethostname
from tempfile import mkdtemp
from time import sleep, time
from urllib2 import urlopen

try:
    import kropotkin
    with open('libraries/python/kropotkin.py', 'r') as f:
        expected_lib_src = f.read()
    actual_lib_src = getsource(kropotkin)
    kropotkin_imported = actual_lib_src == expected_lib_src
except ImportError:
    kropotkin_imported = False
if not kropotkin_imported:
    fail_and_exit("Kropotkin module not installed or out of date.\n"
                + "See libraries/python/README.txt for installation steps.")

from core.deployer.deployer import deploy

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
try:
    KROPOTKIN_DIR=environ['KROPOTKIN_DIR']
except KeyError:
    KROPOTKIN_DIR=mkdtemp()
print "Kropotkin factspace located at: %s" % KROPOTKIN_DIR

try:
    KROPOTKIN_QUEUE=environ['KROPOTKIN_QUEUE']
except KeyError:
    KROPOTKIN_QUEUE=abspath(join('bin', 'boringq'))
    environ['KROPOTKIN_QUEUE'] = KROPOTKIN_QUEUE
print "Using queue executable %s" % KROPOTKIN_QUEUE

def wait_for_http(timeout):
    finish = now() + timeout

    while True:
        try:
            with closing(urlopen(KROPOTKIN_URL)) as k:
                k.read()
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

env = {'KROPOTKIN_URL': KROPOTKIN_URL,
       'KROPOTKIN_DIR': KROPOTKIN_DIR,
       'KROPOTKIN_QUEUE': KROPOTKIN_QUEUE}
http_pid = deploy('http', 'http', env)
if not wait_for_http(10):
    fail_and_exit("Http not starting")
else:
    print "Kropotkin available at %s" % KROPOTKIN_URL

environ['KROPOTKIN_URL'] = KROPOTKIN_URL

elements = [{'type': 'component_available',
              'keys': dumps(['location']),
              'translation': 'Component available in %(location)s'},
            {'type': 'component',
             'keys': dumps(['name', 'bytes', 'language', 'content_type']),
             'translation': 'Component %(name)s, language %(language)s, '\
                          + 'type %(content_type)s'},
            {'type': 'component_deployed',
             'keys': dumps(['name', 'location', 'identifier']),
             'translation': 'Component %(name)s deployed to %(location)s '\
                          + 'with identifier %(identifier)s'},
            {'type': 'factspace_wanted',
              'keys': dumps(['name', 'directory']),
              'translation': 'Factspace %(name)s requested'},
            {'type': 'factspace',
              'keys': dumps(['name', 'directory']),
              'translation': 'Factspace %(name)s created in %(directory)s'},
            {'type': 'library_available',
             'keys': dumps(['directory', 'language']),
             'translation': 'Library available in %(directory)s, ' \
                          + 'language %(language)s'},
            {'type': 'subscription',
             'keys': dumps(['type', 'confidence', 'queue']),
             'translation': 'Subscription to %(confidence)ss ' \
                          + 'of type %(type)s using queue %(queue)s'}]

for e in elements:
    if not kropotkin.store_fact('kropotkin', 'constitution_element', e):
        fail_and_exit('Could not store %s' % e)

content = {'name': 'http', 'location': gethostname(), 'identifier': http_pid}
if not kropotkin.store_fact('kropotkin', 'component_deployed', content):
    fail_and_exit('Cannot store component_deployed fact for http')

for c in dirs_in('core'):
    deploy(c, join('core', c), env)

sleep(1) # Hack to let publisher start

for c in listdir('components'):
    component_location = abspath(join('components', c))
    if not kropotkin.store_fact('kropotkin', 'component_available',
                                {'location': component_location}):
        fail_and_exit('Could not store component_available for %s' % c)

sleep(1) # Hack to let librarian start

for lib in dirs_in('libraries'):
    library_location = abspath(join('libraries', lib))
    if not kropotkin.store_fact('kropotkin', 'library_available',
                                {'directory': library_location,
                                 'language': lib}):
        fail_and_exit('Could not store library_available for %s' % lib)
