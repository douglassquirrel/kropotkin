# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard, urllib2

def _set_up_mock(mb, message_key, response_key, response_text, use_right_correlation_id=True):
    mock_data = {'message_key': message_key, 'response_key': response_key, 'response_content': {'response': response_text}}
    if not use_right_correlation_id:
        mock_data['correlation_id'] = 'wrong_id'
    return mb.post_and_check(post_key='mock', post_content=mock_data, response_key='ready_to_mock.%s' % message_key)

def _send_GET_and_check(mb, GET_path, message_key, response_key, response_text):
    if not _set_up_mock(mb, message_key, response_key, response_text):
        mb.post('Failed to initialise mock')
        return False

    try:
        response = urllib2.urlopen('http://localhost:8080/%s' % GET_path)
    except urllib2.HTTPError, e:
        mb.post("Unexpected HTTP error code %s" % e.code)
        return False

    content_type = response.info().gettype()
    if 'application/json' != content_type:
        print "Wrong content type (should be application/json)"
        return False

    actual_text = json.loads(response.read())
    result = actual_text == response_text
    if not result:
        print "content_type = %s, response = %s" % (content_type, actual_text)
        print "Expected response: %s" % response_text
    return result

def test_converts_http_path_to_message_and_responds(mb):
    return _send_GET_and_check(mb, GET_path='example', message_key='http_GET_request.example', response_key='http_GET_response.example', 
                                  response_text='example response') \
       and _send_GET_and_check(mb, GET_path='second/example', message_key='http_GET_request.second.example', response_key='http_GET_response.second.example',
                                  response_text='another example response')

def test_sends_501_if_no_response(mb):
    try:
        response = urllib2.urlopen('http://localhost:8080/example')
        return False
    except urllib2.HTTPError, e:
        return 501 == e.code

def test_ignores_response_with_wrong_correlation_id(mb):
    _set_up_mock(mb=mb, message_key='http_GET_request.wrong.id', response_key='http_GET_response.wrong.id', 
                 response_text='example.response', use_right_correlation_id=False)

    try:
        response = urllib2.urlopen('http://localhost:8080/wrong/id')
        mb.post("Should not respond with wrong correlation_id")
        return False
    except urllib2.HTTPError, e:
        return True
    finally:
        mb.post(key='http_response_test.stop')

def http_server_test(mb, message):
    result = test_converts_http_path_to_message_and_responds(mb) and \
             test_sends_501_if_no_response(mb)                   and \
             test_ignores_response_with_wrong_correlation_id(mb)
    mb.post(key='http_server_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.core'])
mb.post('process_ready.http_server_test')
mb.start_receive_loop(queue, callback=http_server_test)
