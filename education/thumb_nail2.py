#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys

def search(image, pattern, offset):
    if len(image) <  len(pattern):
        return -1

    for i in xrange(offset, len(image)-len(pattern)):
        image_block = image[i : i+len(pattern)]
        if pattern == image_block:
            return i

    return -1

# f.open, close
# f.read  write
# f.seek, tell
# f.read(), f.readline(), f.read(0, 10)

def read_file(fname):
    h = open(fname, 'rb+')
    image = h.read()
    h.close()
    return image


def write_file(fname, buffer):
    pass


sp = '\xFF\xD8\xFF'
ep = '\xFF\xD9'

img = read_file(fname)
ss = search(image, sp, 1)
ee = search(image, ep, 3)
