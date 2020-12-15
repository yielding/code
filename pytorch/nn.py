#!/usr/bin/env python

import sys
import numpy as np
import matplotlib.pyplot as plt
from   numpy.random import randn

D = np.random.randn(1000, 500)
hidden_layer_sizes = [500]*10
nonlinearities = ['relu']*len(hidden_layer_sizes)

act = {'relu': lambda x: np.maximum(0, x), 'tanh': lambda x: np.tanh(x) }
Hs  = {}

for i in range(len(hidden_layer_sizes)):
    X = D if i == 0 else Hs[i-1]
    fan_i = X.shape[1]
    fan_o = hidden_layer_sizes[i]
    W = np.random.randn(fan_i, fan_o) / np.sqrt(fan_i )

    H = np.dot(X, W)
    H = act[nonlinearities[i]](H)
    Hs[i] = H

print("input layer hand mean %f and std %f" % (np.mean(D), np.std(D)))
layer_means = [np.mean(H) for i, H in Hs.items()]
layer_stds  = [np.std(H)  for i, H in Hs.items()]
for i, H in Hs.items():
    print("hidden layer %d had mean %f and std %f" % (i+1, layer_means[i], layer_stds[i]))

plt.figure()
plt.subplot(121)
plt.plot(Hs.keys(), layer_means, "ob-")
plt.title("layer mean")
plt.subplot(122)
plt.plot(Hs.keys(), layer_stds, "or-")
plt.title("layer std")

plt.figure()
for i,H in Hs.items():
    plt.subplot(1, len(Hs), i+1)
    plt.hist(H.ravel(), 30, range=(-1,1))

plt.show()
