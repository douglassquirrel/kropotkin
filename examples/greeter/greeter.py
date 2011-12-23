# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, unittest

def greet(mb, key, data):
    try:
        response = "Hello, %s!" % data
        mb.post(key="greet-response.%s" % data, data=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

pid = 0
mb = messageboard.MessageBoard(pid)
mb.bind(key='greet')
mb.start_consuming(name='greeter', callback=greet)
