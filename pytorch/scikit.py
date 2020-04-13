#!/usr/bin/env python
# -*- coding: utf-8 -*-


from sklearn import datasets
import numpy as np

iris = datasets.load_iris()
X = iris.data[:, [2,3]]
y = iris.target
print("클래스 레이블:", np.unique(y))
