from glob import glob
from json import dumps, load
from kropotkin import get_newest_fact
from os import environ, rename
from os.path import join
from time import time
from urlparse import parse_qsl

def get_facts(path, params, content):
    factspace, fact_type = path.split('/')[2:4]
    if (factspace == 'kropotkin'):
        facts_dir = environ['KROPOTKIN_DIR']
    else:
        facts_dir = get_newest_fact('kropotkin',
                                    'factspace',
                                    {'name': factspace})['directory']
    facts = _fetch_facts(facts_dir, fact_type, params)
    return 200, dumps(facts), 'application/json'

def _fetch_facts(facts_dir, fact_type, params):
    params = params.copy()
    stamp, result = _extract_kropotkin_criteria(params)
    params.pop('kropotkin_criteria', None)
    timeout = 2000 if (result == 'oldest' or result == 'newest') else 0

    fact_files = []
    finish = __now_millis() + timeout
    while True:
        files = _get_fact_files(facts_dir, fact_type, params, stamp)
        if files or __now_millis() > finish:
            break

    if result == 'oldest':
        files = files[0:1]
    elif result == 'newest':
        files = files[-1:]

    if stamp:
        for i, f in enumerate(files):
            new_name = '.'.join([f, stamp])
            rename(f, new_name)
            files[i] = new_name

    return [_load_fact(f) for f in files]

def _extract_kropotkin_criteria(params):
    stamp = False
    result = 'all'
    try:
        criteria_str = params['kropotkin_criteria']
        criteria_str = criteria_str.replace('-', '=').replace(',', '&')
        criteria = dict(parse_qsl(criteria_str))
        try:
            stamp  = criteria['stamp']
        except KeyError:
            pass
        try:
            result = criteria['result']
        except KeyError:
            pass
    except KeyError:
        pass
    return stamp, result

def _get_fact_files(facts_dir, fact_type, params, stamp):
    files = glob(join(facts_dir, fact_type + ".*"))
    files.sort(key=lambda f: int(f.split('.')[1]))

    if stamp:
        root = stamp.split('.')[0]
        files = [f for f in files if not root in f]

    params_set = set(params.items())
    match = lambda f: params_set < set(_load_fact(f).viewitems())
    return [f for f in files if match(f)]

def _load_fact(fact_filename):
    with open(fact_filename, 'r') as fact_file:
        return load(fact_file)

def __now_millis():
    return int(round(time() * 1000))
