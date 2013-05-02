from errno import EEXIST
from json import loads
from kropotkin import get_newest_fact
from os import environ, makedirs, rename
from os.path import join
from time import time

def store_fact(path, params, content):
    factspace, fact_type = path.split('/')[2:4]

    if not check_fact(factspace, fact_type, content):
        print "Disallowing %s, %s" % (fact_type, content)
        return (400, 'Fact of type %s not allowed by constitution' % fact_type,
                'text/plain')

    if (factspace == 'kropotkin'):
        facts_dir = environ['KROPOTKIN_DIR']
    else:
        facts_dir = get_newest_fact('kropotkin',
                                    'factspace',
                                    {'name': factspace})['directory']

    save_fact(facts_dir, fact_type, content)
    return (200, '', 'text/plain')

def check_fact(factspace, fact_type, content):
    content_dict = loads(content)
    actual_keys = sorted(content_dict.keys())

    if fact_type == 'constitution_element':
        expected_keys = ['keys', 'translation', 'type']
        translation = '''Constitution amended:
  type = %(type)s
  keys = %(keys)s
  translation = %(translation)s
'''
    else:
        constitution_element = get_newest_fact(factspace,
            'constitution_element',
            {'type': fact_type})
        if not constitution_element:
            return False
        expected_keys = sorted(constitution_element['keys'])
        translation = constitution_element['translation']

    if expected_keys != actual_keys:
        return False
    else:
        print ('Storing: ' + translation) % content_dict
        return True

def save_fact(facts_dir, fact_type, content):
    temp_facts_dir = join(facts_dir, 'tmp')
    ensure_exists(temp_facts_dir)

    tstamp = int(time())
    name = '.'.join([fact_type, str(tstamp), str(hash(content)), 'fact'])
    temp_path = join(temp_facts_dir, name)
    real_path = join(facts_dir,      name)

    with open(temp_path, 'w') as fact_file:
        fact_file.write(content)
    rename(temp_path, real_path)

def ensure_exists(directory):
    try:
        makedirs(directory)
    except OSError as exception:
        if exception.errno != EEXIST:
            raise
