#!/usr/bin/env python

import cv2 as cv
import sys
import numpy as np

img = cv.imread(cv.samples.findFile("/Users/yielding/Desktop/blurred.png"))

if img is None:
  sys.exit("Could not read the image.")

cv.imshow("Display window", img)
k = cv.waitKey(0)

