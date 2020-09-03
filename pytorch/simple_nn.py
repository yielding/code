#!/usr/bin/env python
# -*- coding: utf-8 -*-

import numpy as np
from numpy.random import randn

N, D_in, H, D_out = 64, 1000, 100, 10
x, y = randn(N, D_in), randn(N, D_out)

w1, w2 = randn(D_in, H), randn(H, D_out)

for t in range(2000):
    h = 1 / (1 + np.exp(-x.dot(w1)))
    y_pred = h.dot(w2)                    # forward
    loss = np.square(y_pred - y).sum()
    print(t, loss)

    # backward
    grad_y_pred = 2.0 * (y_pred - y)      # loss의 미분(line 15 rhs)
    grad_w2 = h.T.dot(grad_y_pred)
    grad_h  = grad_y_pred.dot(w2.T)
    grad_w1 = x.T.dot(grad_h * h * (1 - h))

    w1 -= 1e-4 * grad_w1
    w2 -= 1e-4 * grad_w2

# 위의 구현은 모듈화가 되어있지 않다.

class MultiplyGate(object):

    """Multiply pattern for computation graph"""

    def __init__(self):
        pass

    def forward(x, y):
        z = x * y
        self.x = x
        self.y = y
        return z
        
    def backward(dz):
        dx = self.y * dz
        dy = self.x * dz
        return [dx, dy]

