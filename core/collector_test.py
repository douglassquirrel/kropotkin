# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def collect_no_message():
    connection = messageboard.get_connection()
    messageboard.bind(connection=connection, key='ready_to_collect.c0m_test_messages_received')
    messageboard.post(connection, 'collect', json.dumps({'messages': ['c0m_1'], 'response': 'c0m_test_messages_received'}))
    (method, body) = messageboard.get_one_message(connection)
    if None == method:
        return False    

    messageboard.bind(connection=connection, key='c0m_test_messages_received')
    (key, body) = messageboard.get_one_message(connection)
    return (None == key)

def collect_one_message():
    connection = messageboard.get_connection()
    messageboard.bind(connection=connection, key='ready_to_collect.c1m_test_messages_received')
    messageboard.post(connection, 'collect', json.dumps({'messages': ['c1m_1'], 'response': 'c1m_test_messages_received'}))
    (method, body) = messageboard.get_one_message(connection)
    if None == method:
        return False    

    messageboard.bind(connection=connection, key='c1m_test_messages_received')
    messageboard.post(connection, 'c1m_1')
    (key, body) = messageboard.get_one_message(connection)
    return ('c1m_test_messages_received' == key)

def collect_two_messages():
    return True

def collector_test(connection, key, body):
    result =   collect_no_message() \
           and collect_one_message() \
           and collect_two_messages()
    messageboard.post(connection, 'collector_test_result', json.dumps(result))

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='process_ready.collector')
messageboard.start_consuming(connection=connection, name='collector_test', callback=collector_test)
