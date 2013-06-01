from errno import EEXIST
from json import loads, dumps
from kropotkin import get_newest_fact
from os import environ, makedirs, rename
from os.path import join
from sqlite3 import connect
from sys import stderr
from time import time

def store_statement(path, params, content, id_generator):
    factspace, confidence, fact_type = path.split('/')[2:5]

    content_dict = loads(content)
    if 'kropotkin_id' in content_dict:
        content_dict['kropotkin_id'] = str(id_generator.next())
        content = dumps(content_dict)

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

    save_statement(statements_dir, confidence, fact_type, content)
    return (200, '', 'text/plain')

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

def save_statement(statements_dir, confidence, fact_type, content):
    save_statement_file(statements_dir, confidence, fact_type, content)
    save_statement_db(statements_dir, confidence, fact_type, content)

def save_statement_file(statements_dir, confidence, fact_type, content):
    temp_statements_dir = join(statements_dir, 'tmp')
    ensure_exists(temp_statements_dir)

    tstamp = int(time())
    name = '.'.join([fact_type, str(tstamp), str(hash(content)), confidence])
    temp_path = join(temp_statements_dir, name)
    real_path = join(statements_dir,      name)

    with open(temp_path, 'w') as statement_file:
        statement_file.write(content)
    rename(temp_path, real_path)

CREATE_TABLE_TEMPLATE = '''CREATE TABLE IF NOT EXISTS %s
                           (ID INTEGER PRIMARY KEY,
                            TIMESTAMP TEXT DEFAULT CURRENT_TIMESTAMP,
                            %s)'''
INSERT_TEMPLATE = '''INSERT INTO %s
                     (%s) VALUES (%s)'''
def save_statement_db(statements_dir, confidence, fact_type, content):
    content_dict = loads(content)
    keys = content_dict.keys()
    value_params = ['?' for key in keys]
    values = [content_dict[key] for key in keys]

    create_table_sql = CREATE_TABLE_TEMPLATE % \
        (fact_type, ', '.join(keys))
    insert_sql = INSERT_TEMPLATE % \
        (fact_type, ','.join(keys), ','.join(value_params))
    connection = connect(join(statements_dir, 'factspace.db'))
    cursor = connection.cursor()
    try:
        cursor.execute(create_table_sql)
        cursor.execute(insert_sql, values)
    except Exception as exception:
        stderr.write('''Exception %s
                        Create table SQL: %s
                        Insert SQL: %s
                        Values: %s
                     ''' % (exception, create_table_sql, insert_sql, values))
    finally:
        connection.commit()
        connection.close()

def ensure_exists(directory):
    try:
        makedirs(directory)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise
