# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard, urllib2

responder_code = """
import messageboard

def respond(mb, message):
    global queue, message_key, response_key, response_text
    if 'http_response_test.stop' == message.key:
        mb.stop_receive_loop()
    elif 'http_response_test.init' == message.key:
        message_key = str(message.content['message_key'])
        response_key = str(message.content['response_key'])
        response_text = str(message.content['response_text'])
        mb.watch_for(keys=[message_key], queue=queue)
        mb.post(key='process_initialised.http_response_test')
    elif response_key and response_text:
        mb.post(key=response_key, content={'response' : response_text}, correlation_id=message.correlation_id)

message_key = None
response_key = None
response_text = None
mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['http_response_test.init', 'http_response_test.stop'])
mb.post(key='process_ready.http_response_test')
mb.start_receive_loop(queue=queue, callback=respond)
"""

def send_GET_and_check(mb, GET_path, message_key, response_key, response_text):
    if not mb.post_and_check(post_key='start_process', post_content={'name': 'http_response_test', 'code': responder_code}, 
                             response_key='process_ready.http_response_test'):
        print "Failed to start http_response_test process"
        return False
    if not mb.post_and_check(post_key='http_response_test.init', 
                             post_content={'message_key': message_key, 'response_key': response_key, 'response_text': response_text},
                             response_key='process_initialised.http_response_test'):
        print "Failed to initialise http_response_test process"
        return False

    try:
        response = urllib2.urlopen('http://localhost:8080/%s' % GET_path)
    except urllib2.HTTPError, e:
        mb.post("Unexpected HTTP error code %s" % e.code)
        return False
    finally:
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
    return send_GET_and_check(mb, GET_path='example', message_key='http_GET_request.example', response_key='http_GET_response.example', 
                                  response_text='example response') \
       and send_GET_and_check(mb, GET_path='second/example', message_key='http_GET_request.second.example', response_key='http_GET_response.second.example',
                                  response_text='another example response')

def test_sends_501_if_no_response(mb):
    try:
        response = urllib2.urlopen('http://localhost:8080/example')
        return False
    except urllib2.HTTPError, e:
        return 501 == e.code

def http_server_test(mb, message):
    result = test_converts_http_path_to_message(mb) and test_sends_501_if_no_response(mb)
    mb.post(key='http_server_test_result', content=result)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['component_ready.core'])
mb.post('process_ready.http_server_test')
mb.start_receive_loop(queue, callback=http_server_test)
