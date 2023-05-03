#!/usr/bin/env python

import unittest

class ArithmaticTest(unittest.TestCase):
    def runTest(self):
        self.failUnless(1+1 == 2, 'one + one failed!')
        self.failIf(1+1 != 2)
        self.failUnlessEqual(1+1, 2)

class ArithmaticTest2(unittest.TestCase):
    def setUp(self):
        print("setUp called")

    def tearDown(self):
        print("tearDown called")

    def testTrue(self):
        self.assertEqual(1, 1)

    def testReallyTrue(self):
        self.assertEqual(1, 1)

def suite():
    suite = unittest.TestSuite()
    suite.addTest(ArithmaticTest())
    suite.addTest(ArithmaticTest2('testTrue'))
    suite.addTest(ArithmaticTest2('testReallyTrue'))
    return suite

def suite2():
    tests = ['testTrue', 'testReallyTrue']
    return unittest.TestSuite(map(ArithmaticTest2, tests))

if __name__ == '__main__':
    runner = unittest.TextTestRunner()
    runner.run(suite())
