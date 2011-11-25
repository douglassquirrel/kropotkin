# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, time, unittest

class TestStartProcess(unittest.TestCase):
    def setUp(self):
        self.create_strings()

    def tearDown(self):
        messageboard.post('stop.start_process_test_echo')

    def test_starts_a_process(self):
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code':self.echo_code}))
        time.sleep(1)
        self.assert_echo_responds_normally()

    def test_sends_process_started_message(self):
        (channel, queue_name) = messageboard.bind('process_started.start_process_test_echo')
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code':self.echo_code}))
        (method, body) = messageboard.get_one_message(channel, queue_name, 'process_started.start_process_test_echo')
        self.assertNotEqual(method, None)

    def test_process_with_typo_does_not_start(self):
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code':self.code_with_typos}))
        time.sleep(1)
        self.assert_echo_does_not_respond()

    def test_start_process_with_typo_then_good_process(self):
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code': self.code_with_typos}))
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code': self.echo_code}))
        time.sleep(2)
        self.assert_echo_responds_normally()

    def test_send_malformed_message_then_start_process(self):
        messageboard.post('start_process', "I am not valid Python code")
        messageboard.post('start_process', str({'verb':'start_process_test_echo', 'code':self.echo_code}))
        time.sleep(1)
        self.assert_echo_responds_normally()

    def assert_echo_responds_with(self, expected_result):
        (channel, queue_name) = messageboard.bind('start_process_test_echo_response')
        messageboard.post('start_process_test_echo', self.text)
        (method, body) = messageboard.get_one_message(channel, queue_name, 'start_process_test_echo_response')
        self.assertEqual(expected_result, body)

    def assert_echo_responds_normally(self):
        self.assert_echo_responds_with(self.text)

    def assert_echo_does_not_respond(self):
        self.assert_echo_responds_with(None)

    def create_strings(self):
        self.echo_code = """
import messageboard
def echo(text):
    print 'Echoing %s' % text
    messageboard.post('start_process_test_echo_response', text)

def run_tests():
    print 'run_tests() has been called'
    messageboard.post('start_process_test_run_tests_called', '')
    return True

messageboard.start_consuming(verb='start_process_test_echo', callback=echo, run_tests=run_tests)
"""
        self.code_with_typos = self.echo_code.replace('e', '$')
        self.text = 'I am a message to be echoed, hear me roar!'

if __name__ == '__main__':
    unittest.main()

