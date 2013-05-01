#!/usr/bin/python
from kropotkin import get_oldest_fact_and_stamp, store_fact
from tempfile import mkdtemp

def create_factspace(name):
    directory = mkdtemp()
    if name != 'kropotkin':
        content = {'name': name, 'directory': directory}
        store_fact('kropotkin', 'factspace', content)
    print 'Storing facts for %s in %s' % (name, directory)
    return directory

if __name__=="__main__":
    while True:
        factspace_fact = get_oldest_fact_and_stamp('kropotkin',
                                                   'factspace-wanted',
                                                   {},
                                                   'factspace.1728')
        if factspace_fact:
            name = factspace_fact['name']
            create_factspace(name)
