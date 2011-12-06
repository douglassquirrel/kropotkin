# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def check_echo_process():
    echo_code = """
import messageboard

def echo(text):
    messageboard.post('__echo_response', text)

messageboard.start_consuming(name='__echo_process', key='__echo', callback=echo)
"""
    text = 'I am a message to be echoed, hear me roar!'

    (channel, queue_name) = messageboard.bind('process_started')
    messageboard.post('start_process', json.dumps({'name': '__echo_process', 'key':'__echo', 'code': echo_code}))
    (method, body) = messageboard.get_one_message(channel, queue_name)
    if not '__echo_process' == body:
        messageboard.post('stop.__echo')
        return False

    (channel, queue_name) = messageboard.bind('__echo_response')
    messageboard.post('__echo', text)
    (method, body) = messageboard.get_one_message(channel, queue_name)
    messageboard.post('stop.__echo')
    return text==body    

def start_process_test(key):
    if key != 'start_process':
        return
    if not check_echo_process():
        result = False
    else:
        messageboard.post('start_process', "I am not valid JSON")
        result = check_echo_process()
    messageboard.post('start_process_test_result', json.dumps(result))

messageboard.start_consuming(name='start_process_test', key='process_ready', callback=start_process_test)
