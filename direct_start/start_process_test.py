# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def check_echo_process(channel):
    echo_code = """
import messageboard

def echo(channel, text):
    messageboard.post(channel, '__echo_response', text)

channel = messageboard.get_connection()
messageboard.start_consuming(channel=channel, name='__echo_process', key='__echo', callback=echo)
"""
    text = 'I am a message to be echoed, hear me roar!'
    
    queue_name = messageboard.bind(channel, 'process_started')
    messageboard.post(channel, 'start_process', json.dumps({'name': '__echo_process', 'key':'__echo', 'code': echo_code}))
    (method, body) = messageboard.get_one_message(channel, queue_name)
    if not '__echo_process' == body:
        messageboard.post(channel, 'stop.__echo')
        return False

    queue_name = messageboard.bind(channel, '__echo_response')
    messageboard.post(channel, '__echo', text)
    (method, body) = messageboard.get_one_message(channel, queue_name)
    messageboard.post(channel, 'stop.__echo')
    return text==body    

def start_process_test(channel, key):
    if key != 'start_process':
        return
    if not check_echo_process(channel):
        result = False
    else:
        messageboard.post(channel, 'start_process', "I am not valid JSON")
        result = check_echo_process(channel)
    messageboard.post(channel, 'start_process_test_result', json.dumps(result))

channel = messageboard.get_connection()
messageboard.start_consuming(channel=channel, name='start_process_test', key='process_ready', callback=start_process_test)
