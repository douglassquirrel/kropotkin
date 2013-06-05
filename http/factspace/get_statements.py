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
    statements = _fetch_statements(statements_dir, factspace, confidence,
                                   fact_type, params)
    return 200, dumps(statements), 'application/json'

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

    return _get_statements_db(statements_dir, factspace, confidence, fact_type,
                              params, stamp, result, number, timeout)

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

def __now_millis():
    return int(round(time() * 1000))
