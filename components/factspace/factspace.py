#!/usr/bin/python
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os.path import join
from tempfile import mkdtemp

def create_factspace(name, directory):
    if not directory:
        directory = mkdtemp()

    content = {'name': name, 'directory': directory}
    if not store_fact('kropotkin', 'factspace', content):
        raise Exception("Cannot store factspace fact")

    return directory

if __name__=="__main__":
    while True:
        factspace_fact = get_oldest_fact_and_stamp('kropotkin',
                                                   'factspace_wanted',
                                                   {},
                                                   'factspace_stamp')
        if factspace_fact:
            create_factspace(factspace_fact['name'],
                             factspace_fact['directory'])
