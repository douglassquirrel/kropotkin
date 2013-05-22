from glob import glob
from json import dumps, load
from kropotkin import get_newest_fact
from os import close, environ, mkdir, O_CREAT, O_EXCL, \
               open as osopen, rename, write
from os.path import exists, join, split
from time import time
from urlparse import parse_qsl

def get_statements(path, params, content, id_generator):
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

    finish = __now_millis() + timeout
    files = []
    while len(files) == 0:
        files = _get_statement_files(statements_dir, confidence, fact_type,
                                     params, stamp)
        if len(files) == 0 and __now_millis() > finish:
            return []

    if result == 'newest':
        files.reverse()

    if result != 'all':
        files = files[0:1]

    if stamp:
        try:
            mkdir(join(statements_dir, 'stamps'))
        except OSError:
            pass

        filtered_files = []
        for f in files:
            stamped_file = _stamped_filename(f, stamp)
            fd = False
            try:
                fd = osopen(stamped_file, O_CREAT | O_EXCL)
                filtered_files.append(f)
            except OSError: #verify errno 17
                pass
            finally:
                if fd:
                    close(fd)
        files = filtered_files

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

def _stamped_filename(f, stamp):
    return join(split(f)[0], 'stamps', '.'.join([split(f)[1], stamp]))

def _get_statement_files(statements_dir, confidence, fact_type, params, stamp):
    files = glob(join(statements_dir, fact_type + ".*." + confidence + "*"))
    files.sort(key=lambda f: int(f.split('.')[1]))

    if stamp:
        files = [f for f in files if not exists(_stamped_filename(f, stamp))]

    matches = lambda f: _dict_match(params, _load_statement(f))
    return [f for f in files if matches(f)]

def _load_statement(filename):
    with open(filename, 'r') as statement_file:
        return load(statement_file)

def __now_millis():
    return int(round(time() * 1000))
