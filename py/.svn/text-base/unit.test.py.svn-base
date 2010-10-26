#!/usr/bin/env python2.4

import random
import unittest
import MyClass

class TestSequenceFunctions(unittest.TestCase):
  def setUp(self):
    self.seq = range(10)

  def testshuffle(self):
    random.shuffle(self.seq)
    self.seq.sort()
    self.assertEqual(self.seq, range(10))
      
  def testchoice(self):
    element = random.choice(self.seq)
    self.assert_(element in self.seq)

  def testsample(self):
    self.assertRaises(ValueError, random.sample, self.seq, 20)
    for element in random.sample(self.seq, 5):
      self.assert_(element in self.seq)
  
  def testMyClass(self):
    cls = MyClass.MyClass("leech")
    self.assert_(cls.pr2() == "leech")

if __name__ == '__main__':
    unittest.main()
