#!/usr/bin/python
from curses import COLOR_BLACK, COLOR_RED, COLOR_WHITE, color_pair, \
                   curs_set, init_pair, wrapper
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

def make_style(n, foreground, background):
    init_pair(n, foreground, background)
    return color_pair(n)

def monitor(screen):
    curs_set(0)
    screen.nodelay(1)

    normal_style = make_style(1, COLOR_BLACK, COLOR_WHITE)
    error_style  = make_style(2, COLOR_RED, COLOR_WHITE)
    screen.bkgd(normal_style)

    error_window = screen.subwin(3, 0)
    error_window.idlok(1)
    error_window.scrollok(1)
    error_window_height = error_window.getmaxyx()[0]

    factspaces = FactCounter('kropotkin', 'factspace', {})
    components = FactCounter('kropotkin', 'component_deployed', {})
    errors     = FactCounter('kropotkin', 'log_data', {'type': 'stderr'})

    screen.addstr(0, 0, '---Monitoring Kropotkin---', normal_style)
    screen.addstr(1, 0, 'q stops monitoring but _not_ Kropotkin itself',
                  normal_style)
    start = time()
    while True:
        factspaces.update()
        components.update()
        error_fact = errors.update()
        if error_fact is not False:
            line = min(errors.count - 1, error_window_height - 1)
            error_text = 'Error in %(component)s - see %(file)s\n' % error_fact
            error_window.addstr(line, 0, error_text, error_style)
            error_window.refresh()

        time_run = int(round(time() - start))
        status_text = 'Factspaces: %d  Components: %d  Errors: %d  Time: %d\r' \
                          % (factspaces.count, components.count, errors.count,
                             time_run)
        screen.addstr(2, 0, status_text, normal_style)
        screen.refresh()

        key = screen.getch()
        if key == ord('q'):
            break

        sleep(1)

wrapper(monitor)
