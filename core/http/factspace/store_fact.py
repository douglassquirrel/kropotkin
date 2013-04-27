from errno import EEXIST
from os import environ, makedirs, rename
from os.path import join
from time import time

def store_fact(path, params, content):
    factspace, fact_type = path.split('/')[2:4]
    if (factspace == 'kropotkin'):
        facts_dir = environ['KROPOTKIN_DIR']
    else:
        facts_dir = get_newest_fact(environ['KROPOTKIN_URL'],
                                    'factspace',
                                    {'name': factspace})['directory']

    save_fact(facts_dir, fact_type, content)
    return '', 'text/plain'

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
