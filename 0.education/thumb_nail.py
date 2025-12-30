#!/usr/bin/env python3
#encoding: utf-8

import sys

def search(image, pattern, offset):
    if len(image) < len(pattern):
        return -1

    for i in range(offset, len(image) - len(pattern)):
        image_block = image[i: i + len(pattern)]
        if pattern == image_block:
            return i

    return -1

def read_file(fname):
    f = open(fname, 'rb+')
    image = f.read()
    f.close()
    return image

def write_file(fname, buffer):
    f = open(fname, 'wb+')
    f.write(buffer)
    f.close()

try:
    beg_pattern, end_pattern = (b'\xFF\xD8\xFF', b'\xFF\xD9')

    img = read_file("../data/taekwon_v.jpg")
    beg = search(img, beg_pattern, 1)
    end = search(img, end_pattern, 3)

    if beg != -1 and end != -1:
       thumbnail = img[beg:end + 2]
       write_file("/Users/yielding/Desktop/1_thumb2.jpg", thumbnail)
       print("Thumbnail is made.")
    else:
       print("No thumbnail available.")

except IOError:
    print >> sys.stderr, "cannot open file"
