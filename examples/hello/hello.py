import messageboard, unittest

def hello(connection, key, name):
    connection = messageboard.get_connection()
    try:
        response = "Hello, %s!" % name
        messageboard.post(connection, key="hello-response.%s" % name, body=response)

    except StandardError as e:
        print "Got exception %s" % str(e)

connection = messageboard.get_connection()
messageboard.bind(connection, key='hello')
messageboard.start_consuming(connection, name='hello', callback=hello)
