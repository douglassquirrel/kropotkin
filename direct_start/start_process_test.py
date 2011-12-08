# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def check_echo_process(connection):
    echo_code = """
import messageboard

def echo(connection, text):
    messageboard.post(connection, '__echo_response', text)

connection = messageboard.get_connection()
messageboard.start_consuming(connection=connection, name='__echo_process', key='__echo', callback=echo)
"""
    text = 'I am a message to be echoed, hear me roar!'
    
    queue_name = messageboard.bind(connection, 'process_started')
    messageboard.post(connection, 'start_process', json.dumps({'name': '__echo_process', 'key':'__echo', 'code': echo_code}))
    (method, body) = messageboard.get_one_message(connection, queue_name)
    if not '__echo_process' == body:
        messageboard.post(connection, 'stop.__echo')
        return False

    queue_name = messageboard.bind(connection, '__echo_response')
    messageboard.post(connection, '__echo', text)
    (method, body) = messageboard.get_one_message(connection, queue_name)
    messageboard.post(connection, 'stop.__echo')
    return text==body    

def start_process_test(connection, key):
    if key != 'start_process':
        return
    if not check_echo_process(connection):
        result = False
    else:
        messageboard.post(connection, 'start_process', "I am not valid JSON")
        result = check_echo_process(connection)
    messageboard.post(connection, 'start_process_test_result', json.dumps(result))

connection = messageboard.get_connection()
messageboard.start_consuming(connection=connection, name='start_process_test', key='process_ready', callback=start_process_test)
