# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import datetime, json, os, pika, time

def json_to_python(s):
    return json.loads(s) if s else None

def python_to_json(x):
    return json.dumps(x) if x else None

def get_connection():
    connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
    channel = connection.channel()
    channel.exchange_declare(exchange='kropotkin', type='topic')
    queue_name = channel.queue_declare(exclusive=True).method.queue
    return {'channel': channel, 'queue_name': queue_name}
    
def bind(connection, key):
    channel, queue_name = connection['channel'], connection['queue_name']
    channel.queue_bind(exchange='kropotkin', queue=queue_name, routing_key=key)

def start_consuming(connection, name, callback):
    channel, queue_name = connection['channel'], connection['queue_name']
    stop_key = 'stop.%s' % name
    def dispatch_message(channel, method, properties, body):
        key = method.routing_key
        data = json_to_python(body)
        if key == stop_key:
            channel.stop_consuming()
            post(connection, key="process_stopped", data=name)
        else:
            callback(connection, key=key, data=data)
    
    bind(connection, key=stop_key)

    channel.basic_consume(dispatch_message, queue=queue_name, no_ack=True)
    post(connection, key="process_ready.%s" % name)
    channel.start_consuming()

def get_message(connection):
    channel, queue_name = connection['channel'], connection['queue_name']
    method, properties, body = channel.basic_get(queue=queue_name, no_ack=True)
    if method.NAME != 'Basic.GetEmpty':
        data = json_to_python(body)
        return (method.routing_key, data)
    else:
        return (None, None)

def get_one_message(connection, seconds_to_wait=10):
    for i in range(seconds_to_wait * 100):
        key, data = get_message(connection)
        if key != None:
            return (key, data)
        time.sleep(0.01)
    return (None, None)

def post(connection, key, data=None):
    body = python_to_json(data)
    channel = connection['channel']
    channel.basic_publish(exchange='kropotkin', routing_key=key, body=body)
    print "PID=%s %s: %s %s" % (os.getpid(), datetime.datetime.now(), key, body)

def post_and_check(connection, post_key, response_key, post_data=None, response_data=None):
    bind(connection, key=response_key)
    post(connection, key=post_key, data=post_data)
    actual_response_key, actual_response_data = get_one_message(connection)
    return response_key == actual_response_key and (response_data==None or actual_response_data == response_data)
    
