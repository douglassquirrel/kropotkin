# Copyright Douglas Squirrel 2011
# This program comes with ABSOLUTELY NO WARRANTY. 
# It is free software, and you are welcome to redistribute it under certain conditions; see the GPLv3 license in the file LICENSE for details.

import messageboard, unittest

class TestRunTests(unittest.TestCase):
    def tearDown(self):
        messageboard.post('stop.__go')

    def test_runs_tests_and_reports_success(self):
        (channel, queue_name) = messageboard.bind('__go_test')
        messageboard.post(verb='process_started', noun='__go')
        (method, body) = messageboard.get_one_message(channel, queue_name, '__go_test')
        self.assertNotEqual(None, method)

if __name__ == '__main__':
    unittest.main()

