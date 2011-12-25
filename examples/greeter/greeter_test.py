# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def test_says_hello_when_given_a_name(mb):
    return mb.post_and_check(post_key='greet', post_content='foo', response_key='greet-response.foo', response_content='Hello, foo!')

def greeter_test(mb, key, content):
    mb = messageboard.MessageBoard()
    result = test_says_hello_when_given_a_name(mb)
    mb.post(key='greeter_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.greeter'])
mb.post(key='process_ready.greeter_test')
mb.start_receive_loop(queue, callback=greeter_test)
