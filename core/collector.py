# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard

global collections
collections = []
def collect(mb, key, content):
    global collections, queue
    try:
        if key == 'collect':
            messages, response = map(str, content['messages']), content['response']
            mb.watch_for(keys=messages, queue=queue)
            statuses = dict(map(lambda x: (x, False), messages))
            collections.append({'statuses': statuses, 'response': response})
            mb.post(key='ready_to_collect.%s' % response)
        else:
            for collection in collections:
                statuses = collection['statuses']
                if key in statuses:
                    statuses[key] = True
            for i, collection in enumerate(collections[:]):
                statuses, response = collection['statuses'], collection['response']
                if all(statuses.values()):
                    mb.post(key=response)
                    collections.pop(i)
            mb.post(key='collector_done_processing.%s' % key)

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
queue = mb.watch_for(keys=['collect'])
mb.post(key='process_ready.collector')
mb.start_receive_loop(queue, callback=collect)
