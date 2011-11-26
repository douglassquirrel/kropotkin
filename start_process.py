# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess

def launch_code(code):
    p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
    p.stdin.write(code)
    p.stdin.close()
    
def start_process(serialised_process_data):
    try:
        process_data = eval(serialised_process_data)
        verb, code, test_code = process_data['verb'], process_data['code'], process_data['test_code']

        print "Starting process for %s" % verb
        launch_code(code)
        
        print "Starting test process for %s" % verb
        launch_code(test_code)
    
        messageboard.post(verb='process_started', noun=verb)
        print "Process started"

    except StandardError as e:
        print "Got exception %s" % str(e)

messageboard.start_consuming(verb='start_process', callback=start_process)
