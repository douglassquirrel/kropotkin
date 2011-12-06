# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import datetime, json, os, pika, time

def connect():
    connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange='kropotkin', type='topic')
    return channel
    
def bind(*routing_keys):
    channel = connect()
    queue_name = channel.queue_declare(exclusive=True).method.queue
    for routing_key in routing_keys:
        channel.queue_bind(exchange='kropotkin', queue=queue_name, routing_key=routing_key)
    return (channel, queue_name)

def start_consuming(name, key, callback):
    stop_key = 'stop.%s' % key
    def dispatch_message(channel, method, properties, body):
        if method.routing_key == key:
            callback(body)
        elif method.routing_key == stop_key:
            channel.stop_consuming()
            post(key="process_stopped", body=key)
        else:
            post(key="unknown_message", body=json.dumps({"key": method.routing_key, "body": body}))

    (channel, queue_name) = bind(key, stop_key)

    channel.basic_consume(dispatch_message, queue=queue_name, no_ack=True)
    post(key="process_ready", body=name)
    channel.start_consuming()

def get_one_message(channel, queue_name, seconds_to_wait=10):
    for i in range(seconds_to_wait):
        method, properties, body = channel.basic_get(queue=queue_name, no_ack=True)
        if method.NAME != 'Basic.GetEmpty':
            return (method, body)
        time.sleep(1)
    return (None, None)

def post(key, body=None):
    channel = connect()    
    channel.basic_publish(exchange='kropotkin', routing_key=key, body=body)
    print "PID=%s %s: %s %s" % (os.getpid(), datetime.datetime.now(), key, body)
