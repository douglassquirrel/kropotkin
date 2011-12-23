# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess
    
def start_process(mb, key, data):
    try:
        name, code = data['name'], data['code']

        p = subprocess.Popen(args="python", stdin=subprocess.PIPE)
        p.stdin.write(code)
        p.stdin.close()
    
        mb.post(key='process_started.%s' % name)

    except StandardError as e:
        print "Got exception %s" % str(e)

pid = 0
mb = messageboard.MessageBoard(pid)
mb.bind(key='start_process')
mb.start_consuming(name='process_starter', callback=start_process)
