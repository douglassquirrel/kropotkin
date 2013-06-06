from errno import EEXIST
from json import loads, dumps
from kropotkin import get_newest_fact
from os import environ, makedirs, rename
from os.path import join
from sqlite3 import connect, OperationalError
from sys import stderr
from time import time

def store_statement(path, params, content, client_ip):
    factspace, confidence, fact_type = path.split('/')[2:5]

    content_dict = loads(content)

    if not check_statement(factspace, fact_type, content_dict):
        stderr.write("Fact of type %s disallowed\n" % fact_type)
        return (400, 'Fact of type %s blocked by constitution\n' % fact_type,
                'text/plain')

    if (factspace == 'kropotkin'):
        statements_dir = environ['KROPOTKIN_DIR']
    else:
        statements_dir = get_newest_fact('kropotkin',
                                         'factspace',
                                         {'name': factspace})['directory']

    rowid = save_statement(statements_dir, confidence, fact_type, content)
    return (200, str(rowid), 'text/plain')

def check_statement(factspace, fact_type, content_dict):
    actual_keys = sorted(content_dict.keys())

    if fact_type == 'constitution_element':
        expected_keys = ['keys', 'translation', 'type']
        translation = "Constitution amended: type = %(type)s, " \
            + "keys = %(keys)s, translation = %(translation)s"
    else:
        constitution_element = get_newest_fact(factspace,
                                               'constitution_element',
                                               {'type': fact_type})
        if not constitution_element:
            return False
        expected_keys = sorted(loads(constitution_element['keys']))

    return expected_keys == actual_keys

CREATE_TABLE_TEMPLATE = '''CREATE TABLE IF NOT EXISTS %s
                           (kropotkin_id INTEGER PRIMARY KEY,
                            kropotkin_timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
                            %s)'''
INSERT_TEMPLATE = '''INSERT INTO %s (%s) VALUES (%s)'''
def save_statement(statements_dir, confidence, fact_type, content):
    content_dict = loads(content)
    content_dict['kropotkin_confidence'] = confidence
    keys = content_dict.keys()
    value_params = ['?' for key in keys]
    values = [content_dict[key] for key in keys]

    create_table_sql = CREATE_TABLE_TEMPLATE % \
        (fact_type, ', '.join(keys))
    insert_sql = INSERT_TEMPLATE % \
        (fact_type, ','.join(keys), ','.join(value_params))
    connection = connect(join(statements_dir, 'factspace.db'))
    try:
        cursor = connection.cursor()
        cursor.execute(create_table_sql)
        cursor.execute(insert_sql, values)
        rowid = cursor.lastrowid
        connection.commit()
    except OperationalError as error:
        stderr.write(('Sqlite error %s\n' \
                    + 'Create table SQL: %s\n' \
                    + 'Insert SQL: %s\n' \
                    + 'Values: %s\n') \
                      % (error, create_table_sql, insert_sql, values))
        rowid = 0
    finally:
        connection.close()

    return rowid

def ensure_exists(directory):
    try:
        makedirs(directory)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise
