#!/usr/bin/python
from kropotkin import get_oldest_fact_and_stamp, store_fact
from tempfile import mkdtemp

def create_factspace(name):
    directory = mkdtemp()
    if name != 'kropotkin':
        content = {'name': name, 'directory': directory}
        if not store_fact('kropotkin', 'factspace', content):
            raise Exception("Cannot store factspace fact")
    print 'Storing facts for %s in %s' % (name, directory)
    return directory

if __name__=="__main__":
    while True:
        factspace_fact = get_oldest_fact_and_stamp('kropotkin',
                                                   'factspace_wanted',
                                                   {},
                                                   'factspace_stamp.1728')
        if factspace_fact:
            name = factspace_fact['name']
            create_factspace(name)
