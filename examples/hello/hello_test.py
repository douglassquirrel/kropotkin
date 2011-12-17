import json, messageboard, unittest

def test_says_hello_when_given_a_name(connection):
    return messageboard.post_and_check(connection, post_key='hello', post_body='foo', response_key='hello-response.foo', response_body='Hello, foo!')

def hello_test(connection, key, body):
    connection = messageboard.get_connection()
    result = test_says_hello_when_given_a_name(connection)
    messageboard.post(connection, key='hello_test_result', body=json.dumps(result))

connection = messageboard.get_connection()
messageboard.bind(connection, key='component_ready.hello')
messageboard.start_consuming(connection, name='hello_test', callback=hello_test)
