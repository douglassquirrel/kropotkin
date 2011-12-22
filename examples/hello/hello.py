# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, unittest

def hello(mb, key, data):
    try:
        response = "Hello, %s!" % data
        mb.post(key="hello-response.%s" % data, data=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

mb = messageboard.MessageBoard()
mb.bind(key='hello')
mb.start_consuming(name='hello', callback=hello)
