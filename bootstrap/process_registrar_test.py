# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

def _no_duplicates(alist):
    return len(set(alist)) == len(alist)

def _register_and_get_id(mb):
    global index
    request_identifier = "%s" % index
    index = index + 1
    queue = mb.watch_for(keys=['process_registered.%s' % request_identifier])
    mb.post(key='register_process', content=request_identifier)
    id = mb.get_one_message(queue).content
    return id

def gives_id(mb):
    return None != _register_and_get_id(mb)

def gives_unique_ids(mb):
    ids = [_register_and_get_id(mb) for i in range(3)]
    return _no_duplicates(ids)

def process_registrar_test(mb, message):
    result = gives_id(mb) and gives_unique_ids(mb)
    mb.post(key='process_registrar_test_result', content=result)

mb = messageboard.MessageBoard()
index = 0
queue = mb.watch_for(keys=['component_ready.process_registrar'])
mb.post(key='process_ready.process_registrar_test')
mb.start_receive_loop(queue=queue, callback=process_registrar_test)
