# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess
    
def register_process(mb, key, content):
    global pid
    try:
        request_identifier = content
        mb.post(key='process_registered.%s' % request_identifier, content=pid)
        pid = pid + 1

    except StandardError as e:
        print "Got exception %s" % str(e)

pid = 1000
mb = messageboard.MessageBoard()
queue = mb.watch_for(key='register_process')
mb.post(key='process_ready.process_registrar')
mb.start_receive_loop(queue=queue, callback=register_process)
