import unittest

class testRecruiter(unittest.TestCase):
    def testDoesNothingIfFull(self):
        counter = MockCounter(always_return=1)
        creator = MockCreator()
        recruiter = Recruiter(expected_count=1, counter=counter, creator=creator)
        self.assertEqual(0, creator.number_created, "Should have created nothing")

class MockCounter:
    def __init__(self, always_return):
        pass

class MockCreator:
    pass

if __name__ == '__main__':
    unittest.main()
