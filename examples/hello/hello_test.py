# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def test_says_hello_when_given_a_name(connection):
    return messageboard.post_and_check(connection, post_key='hello', post_data='foo', response_key='hello-response.foo', response_data='Hello, foo!')

def hello_test(connection, key, data):
    connection = messageboard.get_connection()
    result = test_says_hello_when_given_a_name(connection)
    messageboard.post(connection, key='hello_test_result', data=result)

connection = messageboard.get_connection()
messageboard.bind(connection, key='component_ready.hello')
messageboard.start_consuming(connection, name='hello_test', callback=hello_test)
