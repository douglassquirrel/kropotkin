# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def register_messages(mb, prefix, number):
    response = '%s_test_messages_received' % prefix
    messages = ['%s_%s' % (prefix, i) for i in range(number)]
    return mb.post_and_check(post_key='collect', post_data={'messages': messages, 'response': response}, 
                             response_key='ready_to_collect.%s' % response)

def no_collect_response(mb, message_key):
    return mb.post_and_check(post_key=message_key, response_key='collector_done_processing.%s' % message_key)

def collect_response(mb, message_key, response_key):
    done_key = "collector_done_processing.%s" % message_key
    mb.bind(key=response_key)
    mb.bind(key=done_key)
    mb.post(key=message_key)

    key1, data1 = mb.get_one_message()
    key2, data2 = mb.get_one_message()

    return key1==response_key and key2==done_key

def collect_one_message():
    mb = messageboard.MessageBoard(pid)
    register_messages(mb, prefix='c1m', number=1)
    return collect_response(mb, message_key='c1m_0', response_key='c1m_test_messages_received')

def collect_two_messages():
    mb = messageboard.MessageBoard(pid)
    register_messages(mb, prefix='c2m', number=2)
    return no_collect_response(mb, message_key='c2m_0') \
       and collect_response(mb, message_key='c2m_1', response_key='c2m_test_messages_received')

def collect_interleaved():
    mb = messageboard.MessageBoard(pid)
    register_messages(mb, prefix='ci1', number=2)
    register_messages(mb, prefix='ci2', number=2)
    return no_collect_response(mb, message_key='ci1_0') \
       and no_collect_response(mb, message_key='ci2_0') \
       and collect_response(mb, message_key='ci1_1', response_key='ci1_test_messages_received') \
       and collect_response(mb, message_key='ci2_1', response_key='ci2_test_messages_received') \

def collect_only_once():
    mb = messageboard.MessageBoard(pid)
    register_messages(mb, prefix='c1x', number=1)
    return collect_response(mb, message_key='c1x_0', response_key='c1x_test_messages_received') \
       and no_collect_response(mb, message_key='c1x_0')

def collect_twice():
    mb = messageboard.MessageBoard(pid)
    register_messages(mb, prefix='c2x', number=1)
    if False == collect_response(mb, message_key='c2x_0', response_key='c2x_test_messages_received'):
        return False
    register_messages(mb, prefix='c2x', number=1)
    return collect_response(mb, message_key='c2x_0', response_key='c2x_test_messages_received')

def collector_test(mb, key, data):
    result =   collect_one_message() \
           and collect_two_messages() \
           and collect_interleaved() \
           and collect_only_once() \
           and collect_twice()
    mb.post(key='collector_test_result', data=result)

pid = 0
mb = messageboard.MessageBoard(pid)
mb.bind(key='component_ready.collector')
mb.start_consuming(name='collector_test', callback=collector_test)
