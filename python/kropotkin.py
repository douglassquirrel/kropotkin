from httplib2 import Http
from json import loads, dumps
from os import environ
from urllib import urlencode
from time import time

def store_fact(factspace, type_, content):
    url = '%s/factspace/%s/%s' % (environ['KROPOTKIN_URL'], factspace, type_)
    headers = {'content-type': 'application/x-www-form-urlencoded'}
    Http().request(url, "POST", dumps(content), headers)

def get_oldest_fact_and_stamp(factspace, type_, criteria, stamp):
    criteria = criteria.copy()
    criteria['kropotkin_criteria'] = 'stamp-%s,result-oldest' % stamp
    facts = get_all_facts(factspace, type_, criteria)
    if facts:
        return facts[0]
    else:
        return None

def get_newest_fact(factspace, type_, criteria):
    criteria = criteria.copy()
    criteria['kropotkin_criteria'] = 'result-newest'
    facts = get_all_facts(factspace, type_, criteria)
    if facts:
        return facts[0]
    else:
        return None

def get_all_facts(factspace, type_, criteria):
    url = '%s/factspace/%s/%s?%s' \
        % (environ['KROPOTKIN_URL'], factspace, type_, urlencode(criteria))
    resp, content = Http().request(url)
    if resp.status == 200:
        return loads(content)
    else:
        raise Exception("Unexpected response from server: %d" % resp.status)

def create_factspace(name, timeout=5):
    store_fact('kropotkin', 'factspace_wanted', {'name': name})
    finish = int(round(time())) + timeout
    while int(round(time())) < finish:
        if get_newest_fact('kropotkin', 'factspace', {'name': name}):
            return True
    return False
