# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.
# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import datetime, json, os, pika, time

class MessageBoard:
    def _serialise(self, c):
        return json.dumps(c) if c != None else None

    def _deserialise(self, s):
        return json.loads(s) if not (s in [None, '']) else None

    def __init__(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()

    def post(self, key, content=None):
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=self._serialise(content))
        print "PID=%s %s: %s %s" % (os.getpid(), datetime.datetime.now(), key, content)

    def watch_for(self, key, queue=None):
        if not queue:
            queue = self.channel.queue_declare(exclusive=True).method.queue
        self.channel.queue_bind(exchange='kropotkin', queue=queue, routing_key=key)
        return queue

    def get_one_message(self, queue, seconds_to_wait=1):
        for i in range(100 * seconds_to_wait):
            method, properties, body = self.channel.basic_get(queue=queue, no_ack=True)
            if method.NAME != 'Basic.GetEmpty':
                return (method.routing_key, self._deserialise(body))
            time.sleep(0.01)
        return (None, None)

    def start_receive_loop(self, queue, callback):
        def dispatch_message(channel, method, properties, body):
            callback(self, key=method.routing_key, content=self._deserialise(body))
    
        self.channel.basic_consume(consumer_callback=dispatch_message, queue=queue, no_ack=True)
        self.channel.start_consuming()

    def stop_receive_loop(self):
        self.channel.stop_consuming()

    def post_and_check(self, post_key, response_key, post_content=None, response_content=None):
        queue = self.watch_for(key=response_key)
        self.post(key=post_key, content=post_content)
        actual_response_key, actual_response_content = self.get_one_message(queue)
        return response_key == actual_response_key and (response_content == None or response_content == actual_response_content)
