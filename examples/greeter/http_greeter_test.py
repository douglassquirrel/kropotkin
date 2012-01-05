# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def test_converts_greet_request(mb):
    return _send_and_check_greet(mb, 'albert') and _send_and_check_greet(mb, 'betty')

def _send_and_check_greet(mb, name):
    correlation_id = 'http_greeter_test.%s' % name
    queue = mb.watch_for(keys=['http_GET_response.greet.%s' % name])
    if not mb.post_and_check(post_key='http_GET_request.greet.%s' % name, response_key='greet', response_content=name, 
                             correlation_id='http_greeter_test.%s' % name):
        mb.post(key="Did not convert incoming request as expected")
        return False
    message = mb.get_one_message(queue=queue, correlation_id=correlation_id)
    return message is not None and message.content == {'response': 'Hello, %s!' % name}

def http_greeter_test(mb, message):
    result = test_converts_greet_request(mb)
    mb.post(key='http_greeter_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.greeter'])
mb.post(key='process_ready.http_greeter_test')
mb.start_receive_loop(queue, callback=http_greeter_test)
