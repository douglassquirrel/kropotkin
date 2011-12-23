# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, urllib2

def send_GET_and_check(mb, GET_path, response_key, response_html, response_data=None):
    mb.bind(key=response_key)
    response = urllib2.urlopen('http://localhost:8080/%s' % GET_path)
    actual_html = response.read()
    content_type = response.info().gettype()
    actual_response_key, actual_response_data = mb.get_one_message()
    return 'application/json' == content_type \
       and response_key == actual_response_key \
       and (response_data==None or actual_response_data == response_data) \
       and actual_html == response_html

def test_converts_http_path_to_message(mb):
    return send_GET_and_check(mb, GET_path='example', response_key='http_GET.example', response_html='"/example"') \
       and send_GET_and_check(mb, GET_path='example/path', response_key='http_GET.example.path', response_html='"/example/path"')

def http_server_test(mb, key, data):
    mb = messageboard.MessageBoard()
    result = test_converts_http_path_to_message(mb)
    mb.post(key='http_server_test_result', data=result)

mb = messageboard.MessageBoard()
mb.bind(key='component_ready.core')
mb.start_consuming(name='http_server_test', callback=http_server_test)
