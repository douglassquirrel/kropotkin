#!/usr/bin/python
from collections import namedtuple
from kropotkin import get_all_facts, get_next_fact_noblock
from kropotkin import print_stdout, subscribe
from os import SEEK_END
from os.path import getsize
from socket import getfqdn
from time import sleep, time

class LogFile:
    def __init__(self, component, type_, path):
        self.component = component
        self.type = type_
        self.path = path
        self.position = getsize(path)

    def __hash__(self):
        return hash((self.component, self.type, self.path))

    def __repr__(self):
        return "LogFile(component='%s', type='%s', path='%s', position=%s)" \
            % (self.component, self.type, self.path, self.position)

LOG_FILES = set()
THIS_MACHINE = getfqdn()
POLL_INTERVAL = 1
MAX_FACT_SIZE = 4000

def should_check_component(component_fact):
    location = component_fact['location']
    name     = component_fact['name']
    return location == THIS_MACHINE and name != 'logger'

def store_log_file(component, type_, filename):
    log_file = LogFile(component, type_, filename)
    LOG_FILES.add(log_file)
    print_stdout("Stored %s" % str(log_file))

def store_component_log_files(component_fact):
    component        = component_fact['name']
    stdout           = component_fact['stdout_file']
    stderr           = component_fact['stderr_file']
    store_log_file(component, 'stdout', stdout)
    store_log_file(component, 'stderr', stderr)

def check_log_file(log_file):
    current_size = getsize(log_file.path)
    bytes_to_read = min(current_size - log_file.position, MAX_FACT_SIZE)
    if bytes_to_read == 0:
        return

    with open(log_file.path, 'r') as f:
        f.seek(-bytes_to_read, SEEK_END)
        log_data = f.read(MAX_FACT_SIZE)
        log_file.position += len(log_data)
    print_stdout('Log file %s at %d' % (str(log_file), time()))
    print_stdout(log_data)

subscribe('kropotkin', 'fact', 'component_deployed')
component_facts = get_all_facts('kropotkin', 'component_deployed', {})
map(store_component_log_files, filter(should_check_component, component_facts))

while True:
    target_end = time() + POLL_INTERVAL

    component_fact = get_next_fact_noblock('kropotkin', 'component_deployed')
    if component_fact and should_check_component(component_fact):
        store_component_log_files(component_fact)

    map(check_log_file, LOG_FILES)

    time_left = target_end - time()
    if time_left > 0:
        sleep(time_left)
