# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, pika

class MessageBoard:
    def __init__(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()

    def post(self, title):
        self.channel.basic_publish(exchange='kropotkin', routing_key='post', body=json.dumps({'title': title}))
