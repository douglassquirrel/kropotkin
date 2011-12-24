# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, pika, time

class MessageBoard:
    def __init__(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()

    def post(self, key, content=None):
        if None == content:
            body=json.dumps({'key': key})
        else:
            body=json.dumps({'key': key, 'content': content})
        self.channel.basic_publish(exchange='kropotkin', routing_key='mb.post', body=body)

    def watch_for(self, key):
        queue = self.channel.queue_declare(exclusive=True).method.queue
        self.channel.queue_bind(exchange='kropotkin', queue=queue, routing_key=key)
        return queue

    def get_one_message(self, queue, seconds_to_wait=1):
        for i in range(100 * seconds_to_wait):
            method, properties, body = self.channel.basic_get(queue=queue, no_ack=True)
            if method.NAME != 'Basic.GetEmpty':
                return (method.routing_key, json.loads(body))
            time.sleep(0.01)
        return (None, None)

