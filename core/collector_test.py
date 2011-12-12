# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def register_messages(connection, prefix, number):
    response = '%s_test_messages_received' % prefix
    messages = ['%s_%s' % (prefix, i) for i in range(number)]
    messageboard.bind(connection=connection, key='ready_to_collect.%s' % response)
    messageboard.post(connection=connection, key='collect', body=json.dumps({'messages': messages, 'response': response}))
    (key, body) = messageboard.get_one_message(connection=connection)
    return None != key

def send_message_and_check(connection, message_key, response_key):
    messageboard.bind(connection=connection, key=response_key)
    messageboard.post(connection=connection, key=message_key)
    (actual_response_key, body) = messageboard.get_one_message(connection=connection)
    return (actual_response_key == response_key)

def collect_one_message():
    connection = messageboard.get_connection()
    register_messages(connection=connection, prefix='c1m', number=1)
    return send_message_and_check(connection=connection, message_key='c1m_0', response_key='c1m_test_messages_received')

def collect_two_messages():
    connection = messageboard.get_connection()
    register_messages(connection=connection, prefix='c2m', number=2)
    if True == send_message_and_check(connection=connection, message_key='c2m_0', response_key='c2m_test_messages_received'):
        return False
    return send_message_and_check(connection=connection, message_key='c2m_1', response_key='c2m_test_messages_received')

def collect_interleaved():
    connection = messageboard.get_connection()
    register_messages(connection=connection, prefix='ci1', number=2)
    register_messages(connection=connection, prefix='ci2', number=2)
    if True == send_message_and_check(connection=connection, message_key='ci1_0', response_key='ci1_test_messages_received'):
        return False
    if True == send_message_and_check(connection=connection, message_key='ci2_0', response_key='ci2_test_messages_received'):
        return False
    if False == send_message_and_check(connection=connection, message_key='ci1_1', response_key='ci1_test_messages_received'):
        return False
    return send_message_and_check(connection=connection, message_key='ci2_1', response_key='ci2_test_messages_received')

#def collect_twice() - call again with same messages
#def collect_only_once() - call once, then send same messages without registering again

def collector_test(connection, key, body):
    result =   collect_one_message() \
           and collect_two_messages() \
           and collect_interleaved()
    messageboard.post(connection, 'collector_test_result', json.dumps(result))

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='process_ready.collector')
messageboard.start_consuming(connection=connection, name='collector_test', callback=collector_test)
