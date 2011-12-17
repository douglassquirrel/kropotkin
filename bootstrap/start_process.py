# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess
    
def start_process(connection, key, data):
    try:
        name, code = data['name'], data['code']

        p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
        p.stdin.write(code)
        p.stdin.close()
    
        messageboard.post(connection, key='process_started.%s' % name)

    except StandardError as e:
        print "Got exception %s" % str(e)

connection = messageboard.get_connection()
messageboard.bind(connection, key='start_process')
messageboard.start_consuming(connection, name='start_process', callback=start_process)
