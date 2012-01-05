# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def http_greet(mb, message):
    try:
        name = message.key.split('.')[-1]
        correlation_id = message.correlation_id
        queue = mb.watch_for(keys=['greet-response.%s' % name])
        mb.post(key='greet', content=name, correlation_id=correlation_id)
        response_message = mb.get_one_message(queue)
        mb.post(key='http_GET_response.greet.%s' % name, content = {'response': response_message.content}, correlation_id=correlation_id)

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['http_GET_request.greet.*'])
mb.post(key='process_ready.http_greeter')
mb.start_receive_loop(queue, callback=http_greet)
