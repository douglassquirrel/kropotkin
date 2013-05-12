from httplib2 import Http
from json import loads, dumps
from os import environ
from urllib import urlencode
from time import time

def make_query_function(confidence, stamped, which):
    if stamped:
        return lambda factspace, type_, criteria, stamp: \
            _get_statements(confidence, which, stamp,
                            factspace, type_, criteria)
    else:
        return lambda factspace, type_, criteria: \
            _get_statements(confidence, which, None,
                            factspace, type_, criteria)

get_oldest_fact_and_stamp = make_query_function('fact', True, 'oldest')
get_newest_fact = make_query_function('fact', False, 'newest')
get_all_facts = make_query_function('fact', False, 'all')

def store_fact(factspace, type_, content):
    return _store_statement('fact', factspace, type_, content)

def store_opinion(factspace, type_, content):
    return _store_statement('opinion', factspace, type_, content)

def create_factspace(name, timeout=5):
    if not store_fact('kropotkin', 'factspace_wanted', {'name': name}):
        return False
    finish = int(round(time())) + timeout
    while int(round(time())) < finish:
        if get_newest_fact('kropotkin', 'factspace', {'name': name}):
            return True
    return False

def _get_statements(confidence, which, stamp, factspace, type_, criteria):
    kropotkin_criteria_list = []
    if stamp is not None:
        kropotkin_criteria_list.append('stamp-' + stamp)
    if which != 'all':
        kropotkin_criteria_list.append('result-' + which)

    criteria = criteria.copy()
    if kropotkin_criteria_list:
        criteria['kropotkin_criteria'] = ','.join(kropotkin_criteria_list)
    statements = _get_all_statements(confidence, factspace, type_, criteria)
    if which == 'all':
        return statements
    elif statements:
        return statements[0]
    else:
        return None

def _get_all_statements(confidence, factspace, type_, criteria):
    kropotkin_url = environ['KROPOTKIN_URL']
    criteria = urlencode(criteria)
    url = '%s/factspace/%s/%s/%s?%s' \
        % (kropotkin_url, factspace, confidence, type_, criteria)
    resp, content = Http().request(url)
    if resp.status == 200:
        return loads(content)
    else:
        raise Exception("Unexpected response from server: %d" % resp.status)

def _store_statement(confidence, factspace, type_, content):
    kropotkin_url = environ['KROPOTKIN_URL']
    url = '%s/factspace/%s/%s/%s' \
        % (kropotkin_url, factspace, confidence, type_)
    headers = {'content-type': 'application/x-www-form-urlencoded'}
    resp, content = Http().request(url, "POST", dumps(content), headers)
    return resp.status == 200
