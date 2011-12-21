# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, unittest

def hello(connection, key, data):
    connection = messageboard.get_connection()
    try:
        response = "Hello, %s!" % data
        messageboard.post(connection, key="hello-response.%s" % data, data=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

connection = messageboard.get_connection()
messageboard.bind(connection, key='hello')
messageboard.start_consuming(connection, name='hello', callback=hello)
