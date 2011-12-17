# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def start_echo_process(connection):
    echo_code = """
import messageboard

def echo(connection, key, data):
    messageboard.post(connection, key='__echo_response', data=data)

connection = messageboard.get_connection()
messageboard.bind(connection, key='__echo')
messageboard.start_consuming(connection, name='__echo_process', callback=echo)
"""    
    messageboard.bind(connection, key='process_started.__echo_process')
    messageboard.bind(connection, key='process_ready.__echo_process')
    messageboard.post(connection, key='start_process', data={'name': '__echo_process', 'code': echo_code})
    key1, data1 = messageboard.get_one_message(connection)
    key2, data2 = messageboard.get_one_message(connection)
    return 'process_started.__echo_process' == key1 and 'process_ready.__echo_process' == key2

def check_echo_response(connection):
    text = 'I am a message to be echoed, hear me roar!'
    return messageboard.post_and_check(connection, post_key='__echo', post_data=text, response_key='__echo_response', response_data=text)

def check_echo_process(connection):
    result = start_echo_process(connection) \
         and check_echo_response(connection)
    messageboard.post(connection, key='stop.__echo_process')
    return result    

def check_echo_process_after_bad_process(connection):
    messageboard.post(connection, key='start_process', data="I am not a dictionary")
    return check_echo_process(connection)
    
def start_process_test(connection, key, data):
    connection = messageboard.get_connection()
    result   = check_echo_process(connection) \
           and check_echo_process_after_bad_process(connection)
    messageboard.post(connection, key='start_process_test_result', data=result)

connection = messageboard.get_connection()
messageboard.bind(connection, key='component_ready.start_process')
messageboard.start_consuming(connection, name='start_process_test', callback=start_process_test)
