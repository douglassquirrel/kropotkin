# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard


def start_echo_process(connection):
    echo_code = """
import messageboard

def echo(connection, key, text):
    messageboard.post(connection, '__echo_response', text)

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='__echo')
messageboard.start_consuming(connection=connection, name='__echo_process', callback=echo)
"""    
    messageboard.bind(connection, 'process_started.__echo_process')
    messageboard.post(connection, 'start_process', json.dumps({'name': '__echo_process', 'code': echo_code}))
    (key, body) = messageboard.get_one_message(connection)
    return None != body

def check_echo_response(connection):
    messageboard.bind(connection, 'process_ready.__echo_process')
    (key, body) = messageboard.get_one_message(connection)
    if None == key:
        return false

    text = 'I am a message to be echoed, hear me roar!'
    messageboard.bind(connection, '__echo_response')
    messageboard.post(connection, '__echo', text)
    (key, body) = messageboard.get_one_message(connection)
    return text==body

def check_echo_process(connection):
    result = start_echo_process(connection) \
         and check_echo_response(connection)
    messageboard.post(connection, 'stop.__echo_process')
    return result    

def check_echo_process_after_bad_process(connection):
    messageboard.post(connection, 'start_process', "I am not valid JSON")
    return check_echo_process(connection)
    
def start_process_test(connection, key, body):
    connection = messageboard.get_connection()
    result   = check_echo_process(connection) \
           and check_echo_process_after_bad_process(connection)
    messageboard.post(connection, 'start_process_test_result', json.dumps(result))

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='process_ready.start_process')
messageboard.start_consuming(connection=connection, name='start_process_test', callback=start_process_test)
