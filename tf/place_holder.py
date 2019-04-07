#!/usr/bin/env python
# -*- coding: utf-8 -*-

import tensorflow as tf

a = tf.placeholder("float")
b = tf.placeholder("float")

y = tf.multiply(a, b)

with tf.Session() as sess:
    result = sess.run(y, feed_dict={a: 3, b: 6})
    print(result)
