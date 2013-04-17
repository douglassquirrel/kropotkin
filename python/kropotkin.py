from httplib2 import Http
from json import loads, dumps
from urllib import urlencode

def store_fact(factspace_url, name, content):
    print "storing " + str(content)
    url = factspace_url + '/' + name
    headers = {'content-type':'application/x-www-form-urlencoded'}
    Http().request(url, "POST", dumps(content), headers)

def get_oldest_fact_and_stamp(factspace_url, name, criteria, stamp):
    criteria = criteria.copy()
    criteria['kropotkin_criteria'] = 'stamp-%s,result-oldest' % stamp
    facts = __get_facts(factspace_url, name, criteria)
    if facts:
        return facts[0]
    else:
        return None

def get_newest_fact(factspace_url, name, criteria):
    criteria = criteria.copy()
    criteria['kropotkin_criteria'] = 'result-newest'
    facts = __get_facts(factspace_url, name, criteria)
    if facts:
        return facts[0]
    else:
        return None

def get_all_facts(factspace_url, name, criteria):
    return __get_facts(factspace_url, name, criteria)

def __get_facts(factspace_url, name, criteria):
    url = '%s/%s?%s' % (factspace_url, name, urlencode(criteria))
    resp, content = Http().request(url)
    if resp.status == 200:
        return loads(content)
    else:
        raise Exception("Unexpected response from server: %d" % resp.status)
