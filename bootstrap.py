#!/usr/bin/python
from tempfile import mkdtemp
from httplib2 import Http
from os import listdir
from os.path import isdir, join
from core.deployer.deployer import deploy
from sys import exit
from time import time

PORT=2001
KROPOTKIN_URL="http://localhost:%s" % PORT
KROPOTKIN_DIR=mkdtemp()
print "Storing Kropotkin facts in %s" % KROPOTKIN_DIR

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
