# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import datetime, json, os, pika, time

def get_connection():
    connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange='kropotkin', type='topic')
    return {'channel': channel}
    
def bind(connection, *routing_keys):
    channel = connection['channel']
    queue_name = channel.queue_declare(exclusive=True).method.queue
    for routing_key in routing_keys:
        channel.queue_bind(exchange='kropotkin', queue=queue_name, routing_key=routing_key)
    return queue_name

def start_consuming(connection, name, key, callback):
    channel = connection['channel']
    stop_key = 'stop.%s' % key
    def dispatch_message(channel, method, properties, body):
        if method.routing_key == key:
            callback(connection, body)
        elif method.routing_key == stop_key:
            channel.stop_consuming()
            post(connection, key="process_stopped", body=key)
        else:
            post(connection, key="unknown_message", body=json.dumps({"key": method.routing_key, "body": body}))
    
    queue_name = bind(connection, key, stop_key)

    channel.basic_consume(dispatch_message, queue=queue_name, no_ack=True)
    post(connection, key="process_ready", body=name)
    channel.start_consuming()

def get_one_message(connection, queue_name, seconds_to_wait=10):
    channel = connection['channel']
    for i in range(seconds_to_wait):
        method, properties, body = channel.basic_get(queue=queue_name, no_ack=True)
        if method.NAME != 'Basic.GetEmpty':
            return (method, body)
        time.sleep(1)
    return (None, None)

def post(connection, key, body=None):
    channel = connection['channel']
    channel.basic_publish(exchange='kropotkin', routing_key=key, body=body)
    print "PID=%s %s: %s %s" % (os.getpid(), datetime.datetime.now(), key, body)
