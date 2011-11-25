# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess, time

def start_process(serialised_process_data):
    try:
        process_data = eval(serialised_process_data)
        verb, code = process_data['verb'], process_data['code']

        print "Starting process"
        print "Process code:\n%s" % code

        p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
        p.stdin.write(code)
        p.stdin.close()
    
        messageboard.post(verb='process_started.%s' % verb, noun='')
        print "Process started"

    except StandardError as e:
        print "Got exception %s" % str(e)

def run_tests():
    return True

messageboard.start_consuming(verb='start_process', callback=start_process, run_tests=run_tests)
