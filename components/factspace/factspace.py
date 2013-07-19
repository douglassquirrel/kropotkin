#!/usr/bin/python
from kropotkin import get_next_fact, store_fact, subscribe
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
    subscribe('kropotkin', 'fact', 'factspace_wanted')
    while True:
        factspace_fact = get_next_fact('kropotkin', 'factspace_wanted')
        if factspace_fact:
            create_factspace(factspace_fact['name'],
                             factspace_fact['directory'])
