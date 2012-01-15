# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def mock(mb, message):
    global queue
    if message.key == 'mock':
        message_key = str(message.content['message_key'])
        response_key = str(message.content['response_key'])
        mocks[message_key] = response_key
        mb.watch_for(keys=[message_key], queue=queue)
        mb.post(key='ready_to_mock.%s' % message_key)
    else:
        if message.key in mocks:
            mb.post(key=mocks[message.key])

mocks = {}
mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['mock'])
mb.post(key='process_ready.mocker')
mb.start_receive_loop(queue, callback=mock)
