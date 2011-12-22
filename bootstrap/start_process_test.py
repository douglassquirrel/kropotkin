# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def start_echo_process(mb):
    echo_code = """
import messageboard

def echo(mb, key, data):
    mb.post(key='__echo_response', data=data)

mb = messageboard.MessageBoard()
mb.bind(key='__echo')
mb.start_consuming(name='__echo_process', callback=echo)
"""    
    mb.bind(key='process_started.__echo_process')
    mb.bind(key='process_ready.__echo_process')
    mb.post(key='start_process', data={'name': '__echo_process', 'code': echo_code})
    key1, data1 = mb.get_one_message()
    key2, data2 = mb.get_one_message()
    return 'process_started.__echo_process' == key1 and 'process_ready.__echo_process' == key2

def check_echo_response(mb):
    text = 'I am a message to be echoed, hear me roar!'
    return mb.post_and_check(post_key='__echo', post_data=text, response_key='__echo_response', response_data=text)

def check_echo_process(mb):
    result = start_echo_process(mb) \
         and check_echo_response(mb)
    mb.post(key='stop.__echo_process')
    return result    

def check_echo_process_after_bad_process(mb):
    mb.post(key='start_process', data="I am not a dictionary")
    return check_echo_process(mb)
    
def start_process_test(mb, key, data):
    mb = messageboard.MessageBoard()
    result   = check_echo_process(mb) \
           and check_echo_process_after_bad_process(mb)
    mb.post(key='start_process_test_result', data=result)

mb = messageboard.MessageBoard()
mb.bind(key='component_ready.start_process')
mb.start_consuming(name='start_process_test', callback=start_process_test)
