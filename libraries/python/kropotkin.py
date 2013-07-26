from contextlib import closing
from json import loads, dumps
from os import environ
from subprocess import PIPE, Popen
from time import sleep, time
from urllib import urlencode
from urllib2 import urlopen

LOCAL_SUBSCRIPTIONS = {}
LOCAL_SET_SUBSCRIPTIONS = {}
POLL_DELAY = 0.1

def make_query_function(confidence, which, number):
    return lambda factspace, type_, criteria: \
        _get_statements(confidence, which, number,
                        factspace, type_, criteria)

get_newest_fact = make_query_function('fact', 'newest', 1)
get_all_facts = make_query_function('fact', 'all', None)

def store_fact(factspace, type_, content):
    return _store_statement('fact', factspace, type_, content)

def store_opinion(factspace, type_, content):
    return _store_statement('opinion', factspace, type_, content)

def create_factspace(name, directory=None, timeout=5):
    content = {'name': name, 'directory': directory}
    if not store_fact('kropotkin', 'factspace_wanted', content):
        return False
    finish = int(round(time())) + timeout
    while int(round(time())) < finish:
        if get_newest_fact('kropotkin', 'factspace', {'name': name}):
            return True
    return False

def subscribe(factspace, confidence, type_):
    identifier = _execute_queue_command('create_queue')
    if identifier is False:
        return False

    content = {'type': type_, 'confidence': confidence, 'queue': identifier}
    if not store_fact(factspace, 'subscription', content):
        return False
    LOCAL_SUBSCRIPTIONS[(factspace, confidence, type_)] = identifier
    return True

def get_next_statement(factspace, confidence, type_):
    return _get_next_statement(factspace, confidence, type_, True)

def get_next_statement_noblock(factspace, confidence, type_):
    return _get_next_statement(factspace, confidence, type_, False)

def _get_next_statement(factspace, confidence, type_, block):
    if block is True:
        dequeue_command = 'dequeue'
    else:
        dequeue_command = 'dequeue_noblock'

    identifier = LOCAL_SUBSCRIPTIONS[(factspace, confidence, type_)]
    while True:
        result = _execute_queue_command(dequeue_command, identifier=identifier)
        if result is not False:
            return loads(result)
        elif block == False:
            return False

def get_next_fact(factspace, type_):
    return get_next_statement(factspace, 'fact', type_);
def get_next_fact_noblock(factspace, type_):
    return get_next_statement_noblock(factspace, 'fact', type_);

def get_next_opinion(factspace, type_):
    return get_next_statement(factspace, 'opinion', type_);
def get_next_opinion_noblock(factspace, type_):
    return get_next_statement_noblock(factspace, 'opinion', type_);

def subscribe_sets(factspace, confidence, type_, \
                       set_classify, set_size, patience):
    if not subscribe(factspace, confidence, type_):
        return False
    identifier = (factspace, confidence, type_)
    sets = AgeIndexedDict()
    LOCAL_SET_SUBSCRIPTIONS[identifier] = \
        (set_classify, set_size, patience, sets)
    return True

def now():
    return int(round(time()))

def get_next_set(factspace, confidence, type_):
    identifier = (factspace, confidence, type_)
    set_classify, set_size, patience, sets = LOCAL_SET_SUBSCRIPTIONS[identifier]
    while True:
        if sets.len() > 0 and sets.oldest().patience_has_run_out():
            return sets.pop_oldest().to_list()
        statement = get_next_statement_noblock(factspace, confidence, type_)
        if statement is False:
            sleep(POLL_DELAY)
            continue
        id = set_classify(statement)
        try:
            set_ = sets.get(id)
        except KeyError:
            set_ = StatementSet(set_size, patience)
            sets.add(id, set_)
        set_.add(statement)
        if set_.is_complete():
            sets.remove(id)
            return set_.to_list()

class AgeIndexedDict:
    def __init__(self):
        self.dict = {}
        self.ordered_keys = []

    def len(self):
        return len(self.ordered_keys)

    def oldest(self):
        if self.len() == 0:
            raise IndexError()
        return self.dict[self.ordered_keys[0]]

    def pop_oldest(self):
        if self.len() == 0:
            raise IndexError()
        key = self.ordered_keys.pop(0)
        return self.dict.pop(key)

    def get(self, key):
        return self.dict[key]

    def add(self, key, value):
        self.ordered_keys.append(key)
        self.dict[key] = value

    def remove(self, key):
        self.ordered_keys.remove(key)
        del self.dict[key]

class StatementSet:
    def __init__(self, size_when_full, patience):
        self.size_when_full = size_when_full
        self.deadline = now() + patience
        self.statements = []

    def to_list(self):
        return self.statements

    def is_complete(self):
        return len(self.statements) >= self.size_when_full

    def patience_has_run_out(self):
        return now() > self.deadline

    def add(self, statement):
        self.statements.append(statement)

def _get_statements(confidence, which, number,
                    factspace, type_, criteria):
    kropotkin_criteria_list = []
    if which != 'all':
        kropotkin_criteria_list.append('result-' + which)
    if number is not None:
        kropotkin_criteria_list.append('number-' + str(number))

    criteria = criteria.copy()
    if kropotkin_criteria_list:
        criteria['kropotkin_criteria'] = ','.join(kropotkin_criteria_list)
    statements = _get_all_statements(confidence, factspace, type_, criteria)
    if statements and (number is None or number > 1):
        return statements
    elif statements and number == 1:
        return statements[0]
    else:
        return None

def _http_request(url, data=None):
    with closing(urlopen(url, data)) as r:
        return (r.getcode(), r.read())

def _get_all_statements(confidence, factspace, type_, criteria):
    kropotkin_url = environ['KROPOTKIN_URL']
    params = urlencode(criteria)
    url = '%s/factspace/%s/%s/%s?%s' \
        % (kropotkin_url, factspace, confidence, type_, params)
    status, content = _http_request(url)
    if status == 200:
        return loads(content)
    else:
        raise Exception("Unexpected response from server: %d" % status)

def _store_statement(confidence, factspace, type_, content):
    if type_ != 'constitution_element':
        query_content = {'type': type_, 'confidence': confidence}
        subscriptions = get_all_facts(factspace, 'subscription', query_content)
        if subscriptions:
            for subscription in subscriptions:
                identifier = subscription['queue']
                _execute_queue_command('enqueue', dumps(content), identifier)

    kropotkin_url = environ['KROPOTKIN_URL']
    url = '%s/factspace/%s/%s/%s' \
        % (kropotkin_url, factspace, confidence, type_)
    status, content = _http_request(url, dumps(content))
    return False if status != 200 else int(content)

def _execute_queue_command(command, input_=None, identifier=None):
    args = [environ['KROPOTKIN_QUEUE'], command]
    if identifier is not None:
        args.append(identifier)
    p = Popen(args, stdin=PIPE, stdout=PIPE)
    standard_output, error_output = p.communicate(input_)
    if p.returncode != 0:
        return False
    else:
        return standard_output
