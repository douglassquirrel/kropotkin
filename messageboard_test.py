# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import json, messageboard, os, pika, time, unittest

class TestMessageBoard(unittest.TestCase):
    def setUp(self):
        connection = pika.BlockingConnection(pika.ConnectionParameters(host='localhost'))
        self.channel = connection.channel()
        self.channel.exchange_declare(exchange='kropotkin', type='topic')
        self.queue_name = self.channel.queue_declare(exclusive=True).method.queue
        self.mb = messageboard.MessageBoard()

    def test_posts_without_content(self):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_test_key')
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_another_test_key')        
        self._post_and_check_without_content(key='_test_key') 
        self._post_and_check_without_content(key='_another_test_key') 

    def test_posts_with_content(self):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_test_key')
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_another_test_key')        
        self._post_and_check_with_content(key='_test_key', content='_test_content') 
        self._post_and_check_with_content(key='_another_test_key', content='_another_test_content') 

    def test_posts_with_auto_correlation_id(self):
        mb = messageboard.MessageBoard(314159, 0)
        self._post_and_check_auto_correlation_id(mb, 314159, 0)
        self._post_and_check_auto_correlation_id(mb, 314159, 1)
        self._post_and_check_auto_correlation_id(mb, 314159, 2)
        mb = messageboard.MessageBoard(271828, 103)
        self._post_and_check_auto_correlation_id(mb, 271828, 103)
        mb = messageboard.MessageBoard()
        self._post_and_check_auto_correlation_id(mb, os.getpid(), 0)

    def test_posts_with_specified_correlation_id(self):
        expected_correlation_id = 'test specified correlation id'
        mb = messageboard.MessageBoard(314159, 0)
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_test_key')
        returned_correlation_id = mb.post(key='_test_key', content='_test_content', correlation_id=expected_correlation_id)
        method, properties, body = self._wait_for_message_and_check(expected_key='_test_key', expected_body=json.dumps('_test_content'))
        self.assertEqual(expected_correlation_id, properties.correlation_id)
        self.assertEqual(expected_correlation_id, returned_correlation_id)

    def test_watches_for_and_gets_one_message(self):
        self._watch_for_send_and_check(key='_test_key', content={'datum': '_test_datum'}, correlation_id='_test_correlation_id')
        self._watch_for_send_and_check(key='another_test_key', content=None, correlation_id='_another_test_correlation_id')

    def test_returns_none_if_no_message(self):
        queue = self.mb.watch_for(keys=['_test_key']) 
        message = self.mb.get_one_message(queue)
        self.assertEqual(None, message)

    def test_does_not_wait_if_message_ready(self):
        time_to_run = self._time_get_one_message(send_message=True)
        self.assertTrue(time_to_run < 0.2, 'Time to run of %s was too long' % time_to_run)

    def test_waits_one_second_by_default_before_giving_up(self):
        time_to_run = self._time_get_one_message(send_message=False)
        self.assertTrue(0.8 < time_to_run < 1.2, 'Time to run of %s was not about 1 second' % time_to_run)

    def test_waits_given_time_before_giving_up(self):
        time_to_run = self._time_get_one_message(send_message=False, seconds_to_wait=2)
        self.assertTrue(1.8 < time_to_run < 2.2, 'Time to run of %s was not about 2 seconds' % time_to_run)

    def test_ignores_messages_with_wrong_correlation_id(self):
        key = '_test_id'
        queue = self.mb.watch_for(keys=[key]) 
        properties_with_wrong_id = pika.BasicProperties(correlation_id='wrong.id')
        properties_with_right_id = pika.BasicProperties(correlation_id='right.id')
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=json.dumps('wrong message'), properties=properties_with_wrong_id)
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=json.dumps('right message'), properties=properties_with_right_id)
        message = self.mb.get_one_message(queue=queue, correlation_id='right.id')
        self.assertNotEqual(None, message)
        self.assertEqual('right message', message.content)

    def test_adds_a_key_to_watch_for(self):
        keys = ['_test_key', '_test_key_two']
        queue = self.mb.watch_for(keys) 
        for key in keys:
            self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=None)

        received_keys = [self.mb.get_one_message(queue).key for i in range(2)]
        self.assertEqual(keys, received_keys)

    def test_receives_until_stopped(self):
        received_messages = []
        def callback(mb, message):
            if 'stop' == message.key:
                mb.stop_receive_loop()
            else:
                received_messages.append(message)

        queue = self.mb.watch_for(keys=['_test_key', 'stop'])
        data = ['_datum1', '_datum2', '_datum3']
        for datum in data:
            properties = pika.BasicProperties(correlation_id=datum + '_correlation_id')
            self.channel.basic_publish(exchange='kropotkin', routing_key='_test_key', body=json.dumps(datum), properties=properties)
        self.channel.basic_publish(exchange='kropotkin', routing_key='stop', body=None)
        
        self.mb.start_receive_loop(queue=queue, callback=callback)
        self.assertEqual([('_test_key', datum, datum + '_correlation_id') for datum in data], received_messages)

    def _wait_for_message_and_check(self, expected_key, expected_body, seconds_to_wait=1):
        for i in range(seconds_to_wait * 100):
            method, properties, body = self.channel.basic_get(queue=self.queue_name, no_ack=True)
            if method.NAME != 'Basic.GetEmpty':
                self.assertEqual(expected_key, method.routing_key)
                self.assertEqual(expected_body, body)
                return method, properties, body
            time.sleep(0.01)
        self.fail('Did not get expected message key=%s, body=%s' % (expected_key, expected_body))

    def _post_and_check_without_content(self, key):
        self.mb.post(key=key)
        return self._wait_for_message_and_check(expected_key=key, expected_body='')

    def _post_and_check_with_content(self, key, content):
        self.mb.post(key=key, content=content)
        return self._wait_for_message_and_check(expected_key=key, expected_body=json.dumps(content))

    def _post_and_check_auto_correlation_id(self, mb, process_id, initial_index):
        self.channel.queue_bind(exchange='kropotkin', queue=self.queue_name, routing_key='_test_key')
        returned_correlation_id = mb.post(key='_test_key', content='_test_content')
        method, properties, body = self._wait_for_message_and_check(expected_key='_test_key', expected_body=json.dumps('_test_content'))
        self.assertEqual("%s.%s" % (process_id, initial_index), properties.correlation_id)
        self.assertEqual(returned_correlation_id, properties.correlation_id)

    def _watch_for_send_and_check(self, key, content, correlation_id):
        queue = self.mb.watch_for(keys=[key]) 
        body = json.dumps(content) if None != content else None
        properties = pika.BasicProperties(correlation_id=correlation_id)
        self.channel.basic_publish(exchange='kropotkin', routing_key=key, body=body, properties=properties)
        message = self.mb.get_one_message(queue)
        self.assertEqual(key, message.key)
        self.assertEqual(content, message.content)
        self.assertEqual(correlation_id, message.correlation_id)

    def _time_get_one_message(self, send_message, seconds_to_wait=None):
        queue = self.mb.watch_for(keys=['_test_key']) 
        if send_message:
            self.channel.basic_publish(exchange='kropotkin', routing_key='_test_key', body=json.dumps('test_content'))
        time_before = time.time()
        if seconds_to_wait:
            self.mb.get_one_message(queue=queue, seconds_to_wait=seconds_to_wait)
        else:
            self.mb.get_one_message(queue)
        time_to_run = time.time() - time_before
        return time_to_run

if __name__ == '__main__':
    unittest.main()
