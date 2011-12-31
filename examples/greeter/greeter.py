# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def greet(mb, message):
    try:
        response = "Hello, %s!" % message.content
        mb.post(key="greet-response.%s" % message.content, content=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['greet'])
mb.post(key='process_ready.greeter')
mb.start_receive_loop(queue, callback=greet)
