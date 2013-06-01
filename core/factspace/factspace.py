#!/usr/bin/python
from kropotkin import get_oldest_fact_and_stamp, store_fact
from os.path import join
from sqlite3 import connect, OperationalError
from sys import stderr
from tempfile import mkdtemp

STAMPS_TABLE_SQL = '''CREATE TABLE kropotkin_stamps
                      (stamp_id INTEGER PRIMARY_KEY,
                       timestamp TEXT DEFAULT CURRENT_TIMESTAMP,
                       fact_type TEXT, id INTEGER, stamp TEXT,
                       UNIQUE (fact_type, id, stamp))'''
def create_factspace(name):
    directory = mkdtemp()

    connection = connect(join(directory, 'factspace.db'))
    try:
        cursor = connection.cursor()
        cursor.execute(STAMPS_TABLE_SQL)
        connection.commit()
    except OperationalError as error:
        stderr.write('Cannot create stamps table: %s' % error)
    finally:
        connection.close()

    if name == 'kropotkin':
        print "Kropotkin factspace located at: %s" % directory
    else:
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
            name = factspace_fact['name']
            create_factspace(name)
