#!/usr/bin/env python
# -*- coding: utf-8 -*-

import torch
from torch.autograd import Variable

x = [1.0, 2.0, 3.0]
y = [2.0, 4.0, 6.0]

w = Variable(torch.Tensor([1.0]), requires_grad=True)

def forward(x):
    return x * w

def loss(x, y):
    y_pred = forward(x)
    return (y_pred - y) * (y_pred - y)

print("predict (before training)", 4, forward(4).data[0])

for epoch in range(10):
    for xv, yv in zip(x, y):
        l = loss(xv, yv)
        l.backward()
        print("\tgrad: ", xv, yv, w.grad.data[0])
        w.data = w.data - 0.01 * w.grad.data

        w.grad.data.zero_()

    print("progress:", epoch, l.data[0])

print("predict (after training)", 4, forward(4).data[0])
