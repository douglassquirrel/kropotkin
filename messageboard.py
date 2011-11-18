# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import pika, sys, time

def connect():
    connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange='kropotkin', type='topic')
    return channel
    
def bind(*routing_keys):
    channel = connect()
    result = channel.queue_declare(exclusive=True)
    queue_name = result.method.queue
    for routing_key in routing_keys:
        channel.queue_bind(exchange='kropotkin', queue=queue_name, routing_key=routing_key)
    return (channel, queue_name)

def start_consuming(verb, callback, run_tests):
    tests_key = 'run_tests.%s' % verb
    stop_key = 'stop.%s' % verb
    def dispatch_message(channel, method, properties, body):
        if method.routing_key == verb:
            callback(body)
        elif method.routing_key == tests_key:
            print "Got run_tests message"
            if not run_tests():
                print "Tests failed for %s - stopping process" % verb
                channel.stop_consuming()
        elif method.routing_key == stop_key:
            print "Stopping process for %s messages" % verb
            channel.stop_consuming()
        else:
            print "Unknown message: %r:%r" % (method.routing_key, body)

    (channel, queue_name) = bind(verb, tests_key, stop_key)
    print "Waiting for %s messages. To exit send %s" % (verb, stop_key)

    channel.basic_consume(dispatch_message, queue=queue_name, no_ack=True)
    channel.start_consuming()

def get_one_message(channel, queue_name, verb, seconds_to_wait=10):
    for i in range(seconds_to_wait):
        method, properties, body = channel.basic_get(queue=queue_name, no_ack=True)
        if method.NAME != 'Basic.GetEmpty':
            return (method, body)
        time.sleep(1)
    return (None, None)

def post(verb, noun=None):
    channel = connect()    
    channel.basic_publish(exchange='kropotkin', routing_key=verb, body=noun)
    print "Sent %r %r" % (verb, noun)
