# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard, urllib2

responder_code = """
import messageboard

def respond(mb, key, content):
    global queue, message_key, response_text
    if 'http_response_test.stop' == key:
        mb.stop_receive_loop()
    elif 'http_response_test.init' == key:
        message_key = str(content['message_key'])
        response_text = str(content['response_text'])
        mb.watch_for(keys=[message_key], queue=queue)
        mb.post(key='process_initialised.http_response_test')
    elif message_key and response_text:
        mb.post(key='%s.%s' % (message_key, content['request_id']), content = {'response' : response_text})

message_key = None
response_text = None
mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['http_response_test.init', 'http_response_test.stop'])
mb.post(key='process_ready.http_response_test')
mb.start_receive_loop(queue=queue, callback=respond)
"""

def send_GET_and_check(mb, GET_path, message_key, response_text):
    if not mb.post_and_check(post_key='start_process', post_content={'name': 'http_response_test', 'code': responder_code}, 
                             response_key='process_ready.http_response_test'):
        print "Failed to start http_response_test process"
        return False
    if not mb.post_and_check(post_key='http_response_test.init', post_content={'message_key': message_key, 'response_text': response_text},
                             response_key='process_initialised.http_response_test'):
        print "Failed to initialise http_response_test process"
        return False

    response = urllib2.urlopen('http://localhost:8080/%s' % GET_path)
    mb.post(key='http_response_test.stop')

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

def test_converts_http_path_to_message(mb):
    return send_GET_and_check(mb, GET_path='example', message_key='http_GET.example', response_text='example response') \
       and send_GET_and_check(mb, GET_path='second/example', message_key='http_GET.second.example', response_text='another example response')

def http_server_test(mb, key, content):
    result = test_converts_http_path_to_message(mb)
    mb.post(key='http_server_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.core'])
mb.post('process_ready.http_server_test')
mb.start_receive_loop(queue, callback=http_server_test)
