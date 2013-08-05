#!/usr/bin/python
from curses import curs_set, wrapper
from kropotkin import get_all_facts, get_next_fact_noblock, \
                      subscribe, write_stdout
from time import sleep, time

def matches(fact, criteria):
    return fact is not False \
        and all(item in fact.items() for item in criteria.items())

class FactCounter:
    def __init__(self, factspace, type_, criteria):
        self.factspace = factspace
        self.type_ = type_
        self.criteria = criteria
        subscribe(factspace, 'fact', type_)
        facts = get_all_facts(factspace, type_, criteria)
        self.count = 0 if facts is None else len(facts)

    def update(self):
        fact = get_next_fact_noblock(self.factspace, self.type_)
        if matches(fact, self.criteria):
            self.count+=1
            return fact
        else:
            return False

def monitor(screen):
    curs_set(0)
    screen.nodelay(1)

    factspaces = FactCounter('kropotkin', 'factspace', {})
    components = FactCounter('kropotkin', 'component_deployed', {})
    errors     = FactCounter('kropotkin', 'log_data', {'type': 'stderr'})

    screen.addstr(0, 0, '---Monitoring Kropotkin---')
    screen.addstr(1, 0, 'q stops monitoring but _not_ Kropotkin itself')
    start = time()
    while True:
        factspaces.update()
        components.update()
        error_fact = errors.update()
        if error_fact is not False:
            screen.addstr(3, 0, 'Error in %s - see %s\n' \
                              % (error_fact['component'], error_fact['file']))

        time_run = int(round(time() - start))
        screen.addstr(2, 0,
                      'Factspaces: %d  Components: %d  Errors: %d  Time: %d\r' \
                          % (factspaces.count, components.count, errors.count,
                             time_run))

        key = screen.getch()
        if key == ord('q'):
            break

        sleep(1)

wrapper(monitor)
