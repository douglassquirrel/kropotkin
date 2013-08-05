#!/usr/bin/python
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

factspaces = FactCounter('kropotkin', 'factspace', {})
components = FactCounter('kropotkin', 'component_deployed', {})
errors     = FactCounter('kropotkin', 'log_data', {'type': 'stderr'})

write_stdout('---Monitoring Kropotkin---\n')
write_stdout('Ctrl-C stops monitoring but _not_ Kropotkin itself\n')
start = time()
while True:
    time_run = int(round(time() - start))
    write_stdout('Factspaces: %d  Components: %d  Errors: %d  Time: %d\r' \
                     % (factspaces.count, components.count, errors.count,
                        time_run))
    factspaces.update()
    components.update()
    errors.update()

    sleep(1)
