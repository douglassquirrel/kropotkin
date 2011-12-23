# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import datetime, json, os, pika, time

class MessageBoard:
    def _deserialise(self, s):
        return json.loads(s) if not (s in [None, '']) else None

    def _serialise(self, x):
        return json.dumps(x) if x != None else None

    def __init__(self, pid):
        self.pid = pid
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()
        self.channel.exchange_declare(exchange='kropotkin', type='topic')
        self.queue_name = self.channel.queue_declare(exclusive=True).method.queue
    
    def bind(self, key):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key=key)

    def start_consuming(self, name, callback):
        stop_key = 'stop.%s' % name
        def dispatch_message(channel, method, properties, body):
            key = method.routing_key
            data = self._deserialise(body)
            if key == stop_key:
                channel.stop_consuming()
                self.post(key="process_stopped", data={'name': name, 'id': self.pid})
            else:
                callback(self, key=key, data=data)
    
        self.bind(key=stop_key)
                
        self.channel.basic_consume(dispatch_message, queue=self.queue_name, no_ack=True)
        self.post(key="process_ready.%s" % name, data={'id': self.pid})
        self.channel.start_consuming()

    def get_message(self):
        method, properties, body = self.channel.basic_get(queue=self.queue_name, no_ack=True)
        if method.NAME != 'Basic.GetEmpty':
            data = self._deserialise(body)
            return (method.routing_key, data)
        else:
            return (None, None)

    def get_one_message(self, seconds_to_wait=10):
        for i in range(seconds_to_wait * 100):
            key, data = self.get_message()
            if key != None:
                return (key, data)
            time.sleep(0.01)
        return (None, None)

    def post(self, key, data=None):
        body = self._serialise(data)
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=body)
        print "PID=%s %s: %s %s" % (os.getpid(), datetime.datetime.now(), key, body)
        
    def post_and_check(self, post_key, response_key, post_data=None, response_data=None):
        self.bind(key=response_key)
        self.post(key=post_key, data=post_data)
        actual_response_key, actual_response_data = self.get_one_message()
        return response_key == actual_response_key and (response_data==None or actual_response_data == response_data)
    
