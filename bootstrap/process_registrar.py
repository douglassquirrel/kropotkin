# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, subprocess
    
def register_process(mb, key, data):
    global pid
    try:
        request_identifier = data
        mb.post(key='process_registered.%s' % request_identifier, data=pid)
        pid = pid + 1

    except StandardError as e:
        print "Got exception %s" % str(e)

pid = 1000
mb = messageboard.MessageBoard()
mb.bind(key='register_process')
mb.start_consuming(name='process_registrar', callback=register_process)
