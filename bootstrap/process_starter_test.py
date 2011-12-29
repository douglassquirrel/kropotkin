# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def start_echoer(mb):
    echo_code = """
import messageboard, sys

def echo(mb, key, content):
    mb.post(key='__echo_response', content=content)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['__echo'])
mb.post(key='process_ready.__echoer')
mb.start_receive_loop(queue=queue, callback=echo)
"""    
    queue = mb.watch_for(keys=['process_started.__echoer', 'process_ready.__echoer'])
    mb.post(key='start_process', content={'name': '__echoer', 'code': echo_code})
    message1 = mb.get_one_message(queue)
    message2 = mb.get_one_message(queue)
    return 'process_started.__echoer' == message1.key and 'process_ready.__echoer' == message2.key

def check_echo_response(mb):
    text = 'I am a message to be echoed, hear me roar!'
    return mb.post_and_check(post_key='__echo', post_content=text, response_key='__echo_response', response_content=text)

def check_echo_process(mb):
    result = start_echoer(mb) \
         and check_echo_response(mb)
    mb.post(key='stop.__echoer')
    return result    

def check_echo_process_after_bad_process(mb):
    mb.post(key='start_process', content="I am not a dictionary")
    return check_echo_process(mb)
    
def process_starter_test(mb, key, content):
    result   = check_echo_process(mb) \
           and check_echo_process_after_bad_process(mb)
    mb.post(key='process_starter_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.process_starter'])
mb.post(key='process_ready.process_starter_test')
mb.start_receive_loop(queue=queue, callback=process_starter_test)
