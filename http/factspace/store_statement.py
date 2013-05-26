from errno import EEXIST
from json import loads, dumps
from kropotkin import get_newest_fact
from os import environ, makedirs, rename
from os.path import join
from sys import stderr
from time import time

def store_statement(path, params, content, id_generator):
    factspace, confidence, fact_type = path.split('/')[2:5]

    content_dict = loads(content)
    if 'kropotkin_id' in content_dict:
        content_dict['kropotkin_id'] = str(id_generator.next())
        content = dumps(content_dict)

    translation = check_statement(factspace, fact_type, content_dict)
    if not translation:
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
        expected_keys = sorted(constitution_element['keys'])
        translation = constitution_element['translation']

    return translation if expected_keys == actual_keys else False

def save_statement(statements_dir, confidence, fact_type, content):
    temp_statements_dir = join(statements_dir, 'tmp')
    ensure_exists(temp_statements_dir)

    tstamp = int(time())
    name = '.'.join([fact_type, str(tstamp), str(hash(content)), confidence])
    temp_path = join(temp_statements_dir, name)
    real_path = join(statements_dir,      name)

    with open(temp_path, 'w') as statement_file:
        statement_file.write(content)
    rename(temp_path, real_path)

def ensure_exists(directory):
    try:
        makedirs(directory)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise
