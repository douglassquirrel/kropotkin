from glob import glob
from json import dumps, load
from kropotkin import get_newest_fact
from os import environ, rename
from os.path import join
from time import time
from urlparse import parse_qsl

def get_statements(path, params, content):
    factspace, confidence, fact_type = path.split('/')[2:5]
    if (factspace == 'kropotkin'):
        statements_dir = environ['KROPOTKIN_DIR']
    else:
        factspace_info = get_newest_fact('kropotkin',
                                         'factspace',
                                         {'name': factspace})
        if not factspace_info:
            return (404, 'Could not locate factspace %s' % factspace,
                    'text/plain')
        statements_dir = factspace_info['directory']
    statements = _fetch_statements(statements_dir, confidence,
                                   fact_type, params)
    return 200, dumps(statements), 'application/json'

def _fetch_statements(statements_dir, confidence, fact_type, params):
    params = params.copy()
    stamp, result = _extract_kropotkin_criteria(params)
    params.pop('kropotkin_criteria', None)
    timeout = 2000 if (result == 'oldest' or result == 'newest') else 0

    statement_files = []
    finish = __now_millis() + timeout
    while True:
        files = _get_statement_files(statements_dir, confidence, fact_type,
                                     params, stamp)
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

    return [_load_statement(f) for f in files]

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

def _dict_match(d1, d2):
    d2 = d2.copy()
    for k in d2.keys():
        if k not in d1:
            d2.pop(k)
    return set(d1.items()) <= set(d2.items())

def _get_statement_files(statements_dir, confidence, fact_type, params, stamp):
    files = glob(join(statements_dir, fact_type + ".*." + confidence + "*"))
    files.sort(key=lambda f: int(f.split('.')[1]))

    if stamp:
        root = stamp.split('.')[0]
        files = [f for f in files if not root in f]

    matches = lambda f: _dict_match(params, _load_statement(f))
    return [f for f in files if matches(f)]

def _load_statement(filename):
    with open(filename, 'r') as statement_file:
        return load(statement_file)

def __now_millis():
    return int(round(time() * 1000))
