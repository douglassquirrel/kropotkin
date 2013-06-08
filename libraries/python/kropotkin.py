from contextlib import closing
from json import loads, dumps
from os import environ
from time import time
from urllib import urlencode
from urllib2 import urlopen

def make_query_function(confidence, stamped, which, number):
    if stamped:
        return lambda factspace, type_, criteria, stamp: \
            _get_statements(confidence, which, stamp, number,
                            factspace, type_, criteria)
    else:
        return lambda factspace, type_, criteria: \
            _get_statements(confidence, which, None, number,
                            factspace, type_, criteria)

get_oldest_fact_and_stamp = make_query_function('fact', True, 'oldest', 1)
get_newest_fact = make_query_function('fact', False, 'newest', 1)
get_all_facts = make_query_function('fact', False, 'all', None)

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

def get_my_computer_name():
    kropotkin_url = environ['KROPOTKIN_URL']
    url = '%s/mycomputername' % kropotkin_url
    status, content = _http_request(url)
    if status == 200:
        return content
    else:
        raise Exception("Unexpected response from server: %d" % status)

def _get_statements(confidence, which, stamp, number,
                    factspace, type_, criteria):
    kropotkin_criteria_list = []
    if stamp is not None:
        kropotkin_criteria_list.append('stamp-' + stamp)
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
    kropotkin_url = environ['KROPOTKIN_URL']
    url = '%s/factspace/%s/%s/%s' \
        % (kropotkin_url, factspace, confidence, type_)
    status, content = _http_request(url, dumps(content))
    return False if status != 200 else int(content)
