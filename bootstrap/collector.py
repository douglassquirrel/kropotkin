# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

global collections
collections = []
def collect(connection, key, serialised_data):
    global collections
    try:
        if key == 'collect':
            collection_data = json.loads(serialised_data)
            messages, response = map(str, collection_data['messages']), collection_data['response']
            for message in messages:
                messageboard.bind(connection, key=message)
            statuses = dict(map(lambda x: (x, False), messages))
            collections.append({'statuses': statuses, 'response': response})
            messageboard.post(connection, key='ready_to_collect.%s' % response)
        else:
            for collection in collections:
                statuses = collection['statuses']
                if key in statuses:
                    statuses[key] = True
            for i, collection in enumerate(collections[:]):
                statuses, response = collection['statuses'], collection['response']
                if all(statuses.values()):
                    messageboard.post(connection, key=response)
                    collections.pop(i)
            messageboard.post(connection, key='collector_done_processing.%s' % key)

    except StandardError as e:
        print "Got exception %s" % str(e)

connection = messageboard.get_connection()
messageboard.bind(connection, key='collect')
messageboard.start_consuming(connection, name='collector', callback=collect)
