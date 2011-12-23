# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def _no_duplicates(alist):
    return len(set(alist)) == len(alist)

def _register_and_get_id(mb):
    global index
    request_identifier = "%s.%s" % (mb.pid, index)
    index = index + 1
    mb.bind(key='process_registered.%s' % request_identifier)
    mb.post(key='register_process', data=request_identifier)
    key, id = mb.get_one_message()
    return id

def gives_id(mb):
    return None != _register_and_get_id(mb)

def gives_unique_ids(mb):
    ids = [_register_and_get_id(mb) for i in range(3)]
    return _no_duplicates(ids)

def process_registrar_test(mb, key, data):
    mb = messageboard.MessageBoard()
    result = gives_id(mb) and gives_unique_ids(mb)
    mb.post(key='process_registrar_test_result', data=result)

mb = messageboard.MessageBoard()
index = 0
mb.bind(key='component_ready.process_registrar')
mb.start_consuming(name='process_registrar_test', callback=process_registrar_test)
