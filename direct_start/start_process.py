# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard, subprocess

def launch_code(code):
    p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
    p.stdin.write(code)
    p.stdin.close()
    
def start_process(connection, serialised_process_data):
    try:
        process_data = json.loads(serialised_process_data)
        name, code = process_data['name'], process_data['code']

        launch_code(code)
    
        messageboard.post(connection=connection, key='process_started.%s' % name)

    except StandardError as e:
        print "Got exception %s" % str(e)

import time
time.sleep(1)
connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='start_process')
messageboard.start_consuming(connection=connection, name='start_process', callback=start_process)
