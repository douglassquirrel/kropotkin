# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, mb2, pika, time, unittest

class TestMessageBoard(unittest.TestCase):
    def setUp(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()
        self.channel.exchange_declare(exchange='kropotkin', type='topic')
        self.queue_name = self.channel.queue_declare(exclusive=True).method.queue
        self.mb = mb2.MessageBoard()

    def test_post_without_content(self):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='mb.post')        
        self._post_and_check_without_content(key='_test_key') 
        self._post_and_check_without_content(key='_another_test_key') 

    def test_post_with_content(self):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='mb.post')        
        self._post_and_check_with_content(key='_test_key', content='_test_content') 
        self._post_and_check_with_content(key='_another_test_key', content='_another_test_content') 

    def test_watch_for_and_get_one_message(self):
        mb = mb2.MessageBoard()
        self._watch_for_send_and_check(key='_test_key', content={'datum': '_test_datum'})
        self._watch_for_send_and_check(key='another_test_key', content={'datum': 'another_test_datum'})

    def test_returns_none_if_no_message(self):
        queue = self.mb.watch_for(key='_test_key') 
        received_key, received_content = self.mb.get_one_message(queue)
        self.assertEqual(None, received_key)
        self.assertEqual(None, received_content)

    def _wait_for_message_and_check(self, expected_key, expected_body, seconds_to_wait=1):
        for i in range(seconds_to_wait * 100):
            method, properties, body = self.channel.basic_get(queue=self.queue_name, no_ack=True)
            if method.NAME != 'Basic.GetEmpty':
                self.assertEqual(expected_key, method.routing_key)
                self.assertEqual(expected_body, body)
                return
            time.sleep(0.01)
        self.fail('Did not get expected message key=%s, body=%s' % (expected_key, expected_body))

    def _post_and_check_without_content(self, key):
        self.mb.post(key=key)
        self._wait_for_message_and_check(expected_key='mb.post', expected_body=json.dumps({'key': key}))

    def _post_and_check_with_content(self, key, content):
        self.mb.post(key=key, content=content)
        self._wait_for_message_and_check(expected_key='mb.post', expected_body=json.dumps({'key': key, 'content': content}))

    def _watch_for_send_and_check(self, key, content):
        queue = self.mb.watch_for(key=key) 
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=json.dumps(content))
        received_key, received_content = self.mb.get_one_message(queue)
        self.assertEqual(key, received_key)
        self.assertEqual(content, received_content)

if __name__ == '__main__':
    unittest.main()
