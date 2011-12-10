# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard

def collect(connection, key, serialised_data):
    global response
    try:
        if key == 'collect':
            collection_data = json.loads(serialised_data)
            messages, response = collection_data['messages'], collection_data['response']
            messageboard.bind(connection=connection, key=str(messages[0]))
            messageboard.post(connection, 'ready_to_collect.%s' % response)
        else:
            messageboard.post(connection, response)

    except StandardError as e:
        print "Got exception %s" % str(e)

import time
time.sleep(1)

connection = messageboard.get_connection()
messageboard.bind(connection=connection, key='collect')
messageboard.start_consuming(connection=connection, name='collector', callback=collect)
