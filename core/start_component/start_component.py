#!/usr/bin/python
from glob import glob
from httplib2 import Http
from json import loads
from os import access, environ, path, X_OK
from os.path import isdir, join
from subprocess import Popen
from time import sleep

def start_component(directory, kropotkin_url):
    nodes = set(glob(join(directory, "*")))
    dirs = set([f for f in nodes if isdir(f)])
    files = nodes - dirs

    for d in dirs:
        start_component(join(directory, d), kropotkin_url)

    executables = [f for f in files if access(join(directory, f), X_OK)]
    if (len(executables) != 1):
        print "%d executables in %s" % (len(executables), directory)
    else:
        print "Executing: %s" % executables[0]
        Popen(executables[0], cwd=directory, \
                  env={'KROPOTKIN_URL': kropotkin_url})

if __name__=="__main__":
    KROPOTKIN_URL = environ['KROPOTKIN_URL']
    max_time = 0
    while True:
        sleep(1)
        resp, content = Http().request(KROPOTKIN_URL + '/start-component')
        if resp.status != 200:
            raise Exception('Unable to check %s' % KROPOTKIN_URL)
        requests = loads(content)
        if not requests:
            continue
        latest_request = max(requests, key=lambda r: r['time'])
        if latest_request['time'] > max_time:
            max_time = latest_request['time']
            start_component(latest_request['directory'], KROPOTKIN_URL)
