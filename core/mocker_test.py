# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def _register_mock(message_key, response_key):
    return mb.post_and_check(post_key='mock', post_content={'message_key': message_key, 'response_key': response_key}, 
                             response_key='ready_to_mock.%s' % message_key)

def _send_and_check(message_key, response_key):
    return mb.post_and_check(post_key=message_key, response_key=response_key)

def mock_one_message(mb):
    if not _register_mock(message_key='mock_test_message', response_key='mock_test_response'):
        return False

    return _send_and_check(message_key='mock_test_message', response_key='mock_test_response')
    
def mock_two_messages(mb):
    if not    _register_mock(message_key='first_mock_test_message',  response_key='first_mock_test_response') \
       or not _register_mock(message_key='second_mock_test_message', response_key='second_mock_test_response'):
        return False

    result = _send_and_check(message_key='first_mock_test_message', response_key='first_mock_test_response') and \
             _send_and_check(message_key='second_mock_test_message', response_key='second_mock_test_response')
    return result

def mocker_test(mb, message):
    result = mock_one_message(mb) and mock_two_messages(mb)
    mb.post(key='mocker_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.core'])
mb.post(key='process_ready.mocker_test')
mb.start_receive_loop(queue, callback=mocker_test)
