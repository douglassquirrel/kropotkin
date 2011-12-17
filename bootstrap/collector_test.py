# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def register_messages(connection, prefix, number):
    response = '%s_test_messages_received' % prefix
    messages = ['%s_%s' % (prefix, i) for i in range(number)]
    messageboard.bind(connection, key='ready_to_collect.%s' % response)
    messageboard.post(connection, key='collect', body=json.dumps({'messages': messages, 'response': response}))
    (key, body) = messageboard.get_one_message(connection)
    return None != key

def no_collect_response(connection, message_key):
    done_key = "collector_done_processing.%s" % message_key
    messageboard.bind(connection, key=done_key)
    messageboard.post(connection, key=message_key)
    key, body = messageboard.get_one_message(connection)
    return key==done_key

def collect_response(connection, message_key, response_key):
    done_key = "collector_done_processing.%s" % message_key
    messageboard.bind(connection, key=response_key)
    messageboard.bind(connection, key=done_key)
    messageboard.post(connection, key=message_key)

    key1, body1 = messageboard.get_one_message(connection)
    key2, body2 = messageboard.get_one_message(connection)

    return key1==response_key and key2==done_key

def collect_one_message():
    connection = messageboard.get_connection()
    register_messages(connection, prefix='c1m', number=1)
    return collect_response(connection, message_key='c1m_0', response_key='c1m_test_messages_received')

def collect_two_messages():
    connection = messageboard.get_connection()
    register_messages(connection, prefix='c2m', number=2)
    return no_collect_response(connection, message_key='c2m_0') \
       and collect_response(connection, message_key='c2m_1', response_key='c2m_test_messages_received')

def collect_interleaved():
    connection = messageboard.get_connection()
    register_messages(connection, prefix='ci1', number=2)
    register_messages(connection, prefix='ci2', number=2)
    return no_collect_response(connection, message_key='ci1_0') \
       and no_collect_response(connection, message_key='ci2_0') \
       and collect_response(connection, message_key='ci1_1', response_key='ci1_test_messages_received') \
       and collect_response(connection, message_key='ci2_1', response_key='ci2_test_messages_received') \

def collect_only_once():
    connection = messageboard.get_connection()
    register_messages(connection, prefix='c1x', number=1)
    return collect_response(connection, message_key='c1x_0', response_key='c1x_test_messages_received') \
       and no_collect_response(connection, message_key='c1x_0')

def collect_twice():
    connection = messageboard.get_connection()
    register_messages(connection, prefix='c2x', number=1)
    if False == collect_response(connection, message_key='c2x_0', response_key='c2x_test_messages_received'):
        return False
    register_messages(connection, prefix='c2x', number=1)
    return collect_response(connection, message_key='c2x_0', response_key='c2x_test_messages_received')

def collector_test(connection, key, body):
    result =   collect_one_message() \
           and collect_two_messages() \
           and collect_interleaved() \
           and collect_only_once() \
           and collect_twice()
    messageboard.post(connection, key='collector_test_result', body=json.dumps(result))

connection = messageboard.get_connection()
messageboard.bind(connection, key='component_ready.collector')
messageboard.start_consuming(connection, name='collector_test', callback=collector_test)
