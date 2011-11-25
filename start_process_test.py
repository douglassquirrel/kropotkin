# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, unittest

class TestStartProcess(unittest.TestCase):
    def setUp(self):
        self.create_strings()

    def tearDown(self):
        messageboard.post('stop.__echo')

    def test_starts_a_process(self):
        self.start_echo_process(self.echo_code)
        self.assert_echo_responds_normally()

    def test_handles_malformed_message_then_starts_process(self):
        messageboard.post('start_process', "I am not valid Python code")
        self.start_echo_process(self.echo_code)
        self.assert_echo_responds_normally()

    def start_echo_process(self, code):
        (channel, queue_name) = messageboard.bind('process_started.__echo')
        messageboard.post('start_process', str({'verb':'__echo', 'code': self.echo_code}))
        (method, body) = messageboard.get_one_message(channel, queue_name, 'process_started.__echo')
        self.assertNotEqual(None, method)

    def assert_echo_responds_normally(self):
        (channel, queue_name) = messageboard.bind('__echo_response')
        messageboard.post('__echo', self.text)
        (method, body) = messageboard.get_one_message(channel, queue_name, '__echo_response')
        self.assertEqual(self.text, body)

    def create_strings(self):
        self.echo_code = """
import messageboard
def echo(text):
    print 'Echoing %s' % text
    messageboard.post('__echo_response', text)

def run_tests():
    return True

messageboard.start_consuming(verb='__echo', callback=echo, run_tests=run_tests)
"""
        self.text = 'I am a message to be echoed, hear me roar!'

if __name__ == '__main__':
    unittest.main()

