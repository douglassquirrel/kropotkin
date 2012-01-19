# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.
# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import collections, datetime, json, os, pika, time

Message = collections.namedtuple('Message', ['key', 'content', 'correlation_id'])

class MessageBoard:
    def _serialise(self, c):
        return json.dumps(c) if c != None else None

    def _deserialise(self, s):
        return json.loads(s) if not (s in [None, '']) else None

    def __init__(self, process_id=None, initial_index=0):
        self.process_id = process_id if process_id else os.getpid()
        self.correlation_index = initial_index
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()
        self.channel.exchange_declare(exchange='kropotkin', type='topic')

    def post(self, key, content=None, correlation_id=None):
        if not correlation_id:
            correlation_id = '%s.%s' % (self.process_id, self.correlation_index)
            self.correlation_index += 1

        properties = pika.BasicProperties(correlation_id=correlation_id)
        print "PID=%s %s: %s %s %s" % (os.getpid(), datetime.datetime.now(), key, content, correlation_id)  
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=self._serialise(content), properties=properties)
        return correlation_id

    def watch_for(self, keys, queue=None):
        if not queue:
            queue = self.channel.queue_declare(exclusive=True).method.queue
        for key in keys:
            self.channel.queue_bind(exchange='kropotkin', queue=queue, routing_key=key)
        return queue

    def get_one_message(self, queue, correlation_id = None, seconds_to_wait=1):
        for i in range(100 * seconds_to_wait):
            method, properties, body = self.channel.basic_get(queue=queue, no_ack=True)
            if method.NAME != 'Basic.GetEmpty' and (correlation_id is None or properties.correlation_id == correlation_id):
                return Message(key=method.routing_key, content=self._deserialise(body), correlation_id=properties.correlation_id)
            time.sleep(0.01)
        return None

    def start_receive_loop(self, queue, callback):
        def dispatch_message(channel, method, properties, body):
            message = Message(key=method.routing_key, content=self._deserialise(body), correlation_id=properties.correlation_id)
            callback(self, message)
    
        self.channel.basic_consume(consumer_callback=dispatch_message, queue=queue, no_ack=True)
        self.channel.start_consuming()

    def stop_receive_loop(self):
        self.channel.stop_consuming()

    def post_and_check(self, post_key, response_key, post_content=None, response_content=None, correlation_id=None):
        queue = self.watch_for(keys=[response_key])
        self.post(key=post_key, content=post_content, correlation_id=correlation_id)
        message = self.get_one_message(queue, correlation_id)
        return (message is not None) and (response_key == message.key) and (response_content == None or response_content == message.content)
