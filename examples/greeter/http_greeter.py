# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def http_greet(mb, message):
    try:
        mb.post(key='greet', content='albert')

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['http_GET_request.greet.albert'])
mb.post(key='process_ready.http_greeter')
mb.start_receive_loop(queue, callback=http_greet)
