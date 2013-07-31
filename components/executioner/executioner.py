#!/usr/bin/python
from json import loads
from kropotkin import get_all_facts, get_next_fact, subscribe
from os import kill
from signal import SIGTERM
from socket import getfqdn
from sys import stderr

THIS_MACHINE = getfqdn()

def extract_local_process(queue_identifier):
    queue = loads(queue_identifier)
    if 'host' not in queue or 'pid' not in queue:
        return None
    elif queue['host'] != THIS_MACHINE:
        return None
    else:
        return queue['pid']

def execute_all():
    component_facts = get_all_facts('kropotkin', 'component_deployed',
                                    {'location': THIS_MACHINE})
    subscription_facts = get_all_facts('kropotkin', 'subscription', {})

    for f in component_facts:
        if f['name'] != 'executioner':
            execute_pid(f['identifier'])

    for f in subscription_facts:
        pid = extract_local_process(f['queue'])
        if pid is not None:
            execute_pid(pid)

def execute_pid(pid):
    try:
        kill(pid, SIGTERM)
    except OSError, err:
        stderr.write('Exception when killing %d: %s\n' % (pid, str(err)))

if __name__=="__main__":
    subscribe('kropotkin', 'fact', 'stop_requested')
    while True:
        stop_fact = get_next_fact('kropotkin', 'stop_requested')
        if stop_fact and stop_fact['location'] in [THIS_MACHINE, 'all']:
            identifier = stop_fact['identifier']
            if identifier == 'all':
                execute_all()
                break
            else:
                pid = int(identifier)
                execute_pid(pid)
