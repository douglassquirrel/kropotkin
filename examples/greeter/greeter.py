# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def greet(mb, key, content):
    try:
        response = "Hello, %s!" % content
        mb.post(key="greet-response.%s" % content, content=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(key='greet')
mb.post(key='process_ready.greeter')
mb.start_receive_loop(queue, callback=greet)
