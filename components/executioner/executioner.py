#!/usr/bin/python
from kropotkin import get_next_fact, subscribe
from os import kill
from signal import SIGTERM
from socket import getfqdn

THIS_MACHINE = getfqdn()

def execute_all():
    print "Execute all requested, but not yet implemented"

def execute_pid(pid):
    kill(pid, SIGTERM)

if __name__=="__main__":
    subscribe('kropotkin', 'fact', 'stop_requested')
    while True:
        stop_fact = get_next_fact('kropotkin', 'stop_requested')
        if stop_fact and stop_fact['location'] in [THIS_MACHINE, 'all']:
            identifier = stop_fact['identifier']
            if identifier == 'all':
                execute_all()
            else:
                pid = int(identifier)
                execute_pid(pid)
