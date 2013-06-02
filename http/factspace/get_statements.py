from glob import glob
from json import dumps, load, loads
from kropotkin import get_newest_fact
from os import close, environ, mkdir, O_CREAT, O_EXCL, \
               open as osopen, rename, write
from os.path import basename, exists, join, split
from sqlite3 import connect, OperationalError
from sys import stderr
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
    statements = _fetch_statements(statements_dir, factspace, confidence,
                                   fact_type, params)
    return 200, dumps(statements), 'application/json'

def apply_stamp_file(statements_dir, files, stamp):
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
    return filtered_files

CHECK_TABLE_SQL = '''SELECT name FROM sqlite_master
                     WHERE type='table' AND name=? ''';
STAMPS_TABLE_SQL = '''CREATE TABLE IF NOT EXISTS kropotkin_stamps
                      (stamp_id INTEGER PRIMARY KEY,
                       timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
                       fact_type TEXT, statement_id INTEGER, stamp TEXT,
                       UNIQUE (fact_type, statement_id, stamp))'''
STAMP_INSERT_SQL = '''INSERT INTO kropotkin_stamps
                      (fact_type, statement_id, stamp) VALUES (?, ?, ?)'''
SELECT_TEMPLATE = '''SELECT %s
                     FROM %s AS f
                     %s /* stamp clause */
                     %s /* WHERE or AND params */
                     ORDER BY f.kropotkin_timestamp %s
                     %s /* LIMIT number */'''
STAMP_CLAUSE_TEMPLATE = '''LEFT JOIN kropotkin_stamps AS s
                           ON (f.kropotkin_id = s.statement_id
                               AND s.stamp = ?) /* stamp */
                           WHERE s.stamp_id IS NULL'''
KROPOTKIN_KEYS = ['kropotkin_id', 'kropotkin_timestamp', 'kropotkin_confidence']
def _get_statements_db(statements_dir, factspace, confidence, fact_type,
                       params, stamp, result, number, timeout):
    if fact_type == 'constitution_element':
        type_keys = ['keys', 'translation', 'type']
    else:
        constitution_element = get_newest_fact(factspace,
                                               'constitution_element',
                                               {'type': fact_type})
        if not constitution_element:
            raise Exception('queried type that does not exist: %s' % fact_type)
        type_keys = loads(constitution_element['keys'])
    type_keys.extend(KROPOTKIN_KEYS)
    type_keys.sort()

    connection = connect(join(statements_dir, 'factspace.db'))
    cursor = connection.cursor()
    cursor.execute(CHECK_TABLE_SQL, (fact_type,))
    if 0 == len(cursor.fetchall()):
        connection.close()
        return []
    cursor.execute(STAMPS_TABLE_SQL)
    connection.commit()

    if confidence != 'statement':
        params['kropotkin_confidence'] = confidence

    if stamp:
        stamp_clause = STAMP_CLAUSE_TEMPLATE
        match_clause = 'AND '
        values = [stamp]
    else:
        stamp_clause = ''
        match_clause = 'WHERE '
        values = []

    param_keys = sorted(params.keys())
    if param_keys:
        match_clause += ' AND '.join(['%s = ?' % key for key in param_keys])
    else:
        match_clause = ''
    values.extend([params[key] for key in param_keys])
    order = 'DESC' if result == 'newest' else 'ASC'
    limit = 'LIMIT %d' % number if number else ''
    select_sql = SELECT_TEMPLATE % (','.join(type_keys), fact_type,
                                    stamp_clause, match_clause, order, limit)

    finish = __now_millis() + timeout
    while True:
        try:
            cursor.execute(select_sql, values)
            statements = [dict(zip(type_keys, r)) for r in cursor.fetchall()]
            if statements and stamp:
                stamp_values = [(fact_type, s['kropotkin_id'], stamp) \
                                    for s in statements]
                cursor.executemany(STAMP_INSERT_SQL, stamp_values)
            connection.commit()
        except OperationalError as error:
            stderr.write('Sqlite error %s\n' % error)
            statements = []
            break
        if len(statements) > 0 or __now_millis() > finish:
            break

    connection.close()
    return statements

def _fetch_statements(statements_dir, factspace, confidence, fact_type, params):
    params = params.copy()
    stamp, result, number = _extract_kropotkin_criteria(params)
    params.pop('kropotkin_criteria', None)
    timeout = 2000 if (result == 'oldest' or result == 'newest') else 0

    db_statements = \
        _get_statements_db(statements_dir, factspace, confidence, fact_type,
                           params, stamp, result, number, timeout)

    finish = __now_millis() + timeout
    files = []
    while len(files) == 0:
        files = _get_statement_files(statements_dir, confidence, fact_type,
                                     params, stamp)
        if len(files) == 0 and __now_millis() > finish:
            return []

    if result == 'newest':
        files.reverse()

    if number is not None:
        files = files[0:number]

    if stamp:
        files = apply_stamp_file(statements_dir, files, stamp)

    file_statements = [_load_statement(f) for f in files]
    return file_statements

def _extract_kropotkin_criteria(params):
    stamp = False
    result = 'all'
    number = None
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
        try:
            number = int(criteria['number'])
        except KeyError:
            pass
    except KeyError:
        pass
    return stamp, result, number

def _dict_match(d1, d2):
    d2 = d2.copy()
    for k in d2.keys():
        if k not in d1:
            d2.pop(k)
    return set(d1.items()) <= set(d2.items())

def _stamped_filename(f, stamp):
    return join(split(f)[0], 'stamps', '.'.join([split(f)[1], stamp]))

def _get_statement_files(statements_dir, confidence, fact_type, params, stamp):
    if confidence == 'statement':
        confidence = '*'
    files = glob(join(statements_dir, fact_type + ".*." + confidence + "*"))
    files.sort(key=lambda f: int(f.split('.')[1]))

    if stamp:
        files = [f for f in files if not exists(_stamped_filename(f, stamp))]

    matches = lambda f: _dict_match(params, _load_statement(f))
    return [f for f in files if matches(f)]

def _load_statement(filename):
    with open(filename, 'r') as statement_file:
        statement = load(statement_file)
        statement['kropotkin_confidence'] = basename(filename).split('.')[3]
        return statement

def __now_millis():
    return int(round(time() * 1000))
