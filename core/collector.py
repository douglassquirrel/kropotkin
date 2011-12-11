# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

global statuses, response
def collect(connection, key, serialised_data):
    global statuses, response
    try:
        if key == 'collect':
            collection_data = json.loads(serialised_data)
            messages, response = map(str, collection_data['messages']), collection_data['response']
            for message in messages:
                messageboard.bind(connection=connection, key=message)
            statuses = dict(map(lambda x: (x, False), messages))
            messageboard.post(connection, 'ready_to_collect.%s' % response)
        else:
            if key in statuses:
                statuses[key] = True
            if all(statuses.values()):
                messageboard.post(connection, response)

    except StandardError as e:
        print "Got exception %s" % str(e)

import time
time.sleep(1)

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='collect')
messageboard.start_consuming(connection=connection, name='collector', callback=collect)
