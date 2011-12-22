# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def test_says_hello_when_given_a_name(mb):
    return mb.post_and_check(post_key='hello', post_data='foo', response_key='hello-response.foo', response_data='Hello, foo!')

def hello_test(mb, key, data):
    mb = messageboard.MessageBoard()
    result = test_says_hello_when_given_a_name(mb)
    mb.post(key='hello_test_result', data=result)

mb = messageboard.MessageBoard()
mb.bind(key='component_ready.hello')
mb.start_consuming(name='hello_test', callback=hello_test)
