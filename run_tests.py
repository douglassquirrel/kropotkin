# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def run_tests(verb):
    print "Running tests for %s" % verb
    messageboard.post(verb='%s_test' % verb, noun='')

messageboard.start_consuming(verb='process_started', callback=run_tests)
