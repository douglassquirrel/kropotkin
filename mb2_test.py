# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, mb2, pika, time, unittest

class TestMessageBoard(unittest.TestCase):
    def _wait_for_message_and_check(self, expected_key, expected_body, seconds_to_wait=1):
        for i in range(seconds_to_wait * 100):
            method, properties, body = self.channel.basic_get(queue=self.queue_name, no_ack=True)
            if method.NAME != 'Basic.GetEmpty':
                self.assertEqual(expected_key, method.routing_key)
                self.assertEqual(expected_body, body)
                return
            time.sleep(0.01)
        self.fail('Did not get expected message key=%s, body=%s' % (expected_key, expected_body))

    def _post_and_check(self, mb, title):
        mb.post(title=title)
        self._wait_for_message_and_check(expected_key='post', expected_body=json.dumps({'title': title}))

    def setUp(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()
        self.channel.exchange_declare(exchange='kropotkin', type='topic')
        self.queue_name = self.channel.queue_declare(exclusive=True).method.queue

    def test_post_without_data(self):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='post')        
        mb = mb2.MessageBoard()
        self._post_and_check(mb, title='_test_title') 
        self._post_and_check(mb, title='_another_test_title') 

if __name__ == '__main__':
    unittest.main()
