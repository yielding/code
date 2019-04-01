#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np

def _nand(x1, x2):
    x = np.array([x1, x2])
    w = np.array([-0.5, -0.5])
    b = 0.7

    tmp = np.sum(x*w) + b

    return 0 if tmp <= 0 else 1

def _or(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.2

    tmp = np.sum(x*w) + b

    return 0 if tmp <= 0 else 1

def _and(x1, x2):
    x = np.array([x1, x2])
    w = np.array([0.5, 0.5])
    b = -0.7

    tmp = np.sum(x*w) + b

    return 0 if tmp <= 0 else 1

def _xor(x1, x2):
  s1 = _nand(x1, x2)
  s2 = _or(x1, x2)

  return _and(s1, s2)


print(_xor(1, 0))
