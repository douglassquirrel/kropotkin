#!/usr/bin/python
from sys import argv, exit, stderr, version, version_info

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
from subprocess import call
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
environ['KROPOTKIN_URL'] = KROPOTKIN_URL

try:
    KROPOTKIN_QUEUE=environ['KROPOTKIN_QUEUE']
except KeyError:
    KROPOTKIN_QUEUE=abspath(join('bin', 'boringq'))
    environ['KROPOTKIN_QUEUE'] = KROPOTKIN_QUEUE
print "Using queue executable %s" % KROPOTKIN_QUEUE

if len(argv) > 1 and argv[1] == 'stop':
    print 'Stopping Kropotkin instance running on %s' % KROPOTKIN_URL
    content = {'location': 'all', 'identifier': 'all'}
    if not kropotkin.store_fact('kropotkin', 'stop_requested', content):
        fail_and_exit('Could not store stop_requested fact')
    print 'Kropotkin stopping'
    exit(0)

try:
    KROPOTKIN_DIR=environ['KROPOTKIN_DIR']
except KeyError:
    KROPOTKIN_DIR=mkdtemp()
    environ['KROPOTKIN_DIR'] = KROPOTKIN_DIR
print "Kropotkin factspace located at: %s" % KROPOTKIN_DIR

if 'KROPOTKIN_CAN_BUILD' not in environ:
    environ['KROPOTKIN_CAN_BUILD'] = 'python,javascript'

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

def wait_for_fact(factspace, type_, criteria, timeout):
    finish = now() + timeout
    while now() < finish:
        result = kropotkin.get_newest_fact(factspace, type_, criteria)
        if result is not None:
            return True
    return False

def dirs_in(parent):
    parent = abspath(parent)
    return [d for d in listdir(parent) if isdir(join(parent, d))]

http_content = deploy('http', 'http',
                      store_deployed_fact=False, temp_output=True)
if not wait_for_http(10):
    fail_and_exit("Http not starting")

elements = [{'type': 'component_available',
              'keys': dumps(['location']),
              'translation': 'Component available in %(location)s'},
            {'type': 'component',
             'keys': dumps(['name', 'bytes', 'language', 'content_type']),
             'translation': 'Component %(name)s, language %(language)s, '\
                          + 'type %(content_type)s'},
            {'type': 'component_deployed',
             'keys': dumps(['name', 'location', 'identifier',
                            'stdout_file', 'stderr_file']),
             'translation': 'Component %(name)s deployed to %(location)s '\
                          + 'with identifier %(identifier)s'},
            {'type': 'home_component',
             'keys': dumps(['name']),
             'translation': 'The home component is %(name)s'},
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
            {'type': 'log_data',
              'keys': dumps(['component', 'type', 'file', 'data']),
              'translation': 'Logging of type %(type)s from %(component)s '\
                           + 'in file %(file)s: %(data)s'},
            {'type': 'stop_requested',
              'keys': dumps(['location', 'identifier']),
              'translation': 'Component stop requested for location '\
                           + '%(location)s and identifier %(identifier)s'},
            {'type': 'subscription',
             'keys': dumps(['type', 'confidence', 'queue']),
             'translation': 'Subscription to %(confidence)ss ' \
                          + 'of type %(type)s using queue %(queue)s'}]

for e in elements:
    if not kropotkin.store_fact('kropotkin', 'constitution_element', e):
        fail_and_exit('Could not store %s' % e)

if not kropotkin.store_fact('kropotkin', 'component_deployed', http_content):
    fail_and_exit('Cannot store component_deployed fact for http')
if not kropotkin.store_fact('kropotkin', 'factspace',
                            {'name': 'kropotkin', 'directory': KROPOTKIN_DIR}):
    fail_and_exit('Cannot store factspace fact for kropotkin')

for c in dirs_in('core'):
    deploy(c, join('core', c), temp_output=True)

publisher_started = wait_for_fact('kropotkin', 'subscription',
                                  {'type': 'component_available'}, 10)
if publisher_started is False:
    fail_and_exit('Publisher failed to start')

for c in listdir('components'):
    component_location = abspath(join('components', c))
    if not kropotkin.store_fact('kropotkin', 'component_available',
                                {'location': component_location}):
        fail_and_exit('Could not store component_available for %s' % c)

librarian_started = wait_for_fact('kropotkin', 'subscription',
                                  {'type': 'library_available'}, 10)
if librarian_started is False:
    fail_and_exit('Librarian failed to start')

for lib in dirs_in('libraries'):
    library_location = abspath(join('libraries', lib))
    if not kropotkin.store_fact('kropotkin', 'library_available',
                                {'directory': library_location,
                                 'language': lib}):
        fail_and_exit('Could not store library_available for %s' % lib)

if not kropotkin.store_fact('kropotkin', 'home_component',
                            {'name': 'kropotkin_home.html'}):
    fail_and_exit('Could not store home_component')

print "Kropotkin available at %s" % KROPOTKIN_URL
print "To stop Kropotkin, run the command %s stop" % argv[0]

choice = raw_input("1. Start monitor 2. Exit ")
if choice == '1':
    call(join('bin', 'monitor.py'))
