# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess
    
def start_process(mb, key, content):
    try:
        name, code = content['name'], content['code']

        pid = 1000
        p = subprocess.Popen(args=["python", "-", str(pid)], stdin=subprocess.PIPE)
        p.stdin.write(code)
        p.stdin.close()
    
        mb.post(key='process_started.%s' % name, content={'id': pid})

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(key='start_process')
mb.post(key='process_ready.process_starter')
mb.start_receive_loop(queue=queue, callback=start_process)
