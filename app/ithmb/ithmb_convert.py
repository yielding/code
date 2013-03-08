#!/usr/bin/env python
from PIL import Image

file = open('05.bin', 'rb')
raw  = file.read()
file.close()

img_size = (160, 158)
img = Image.fromstring('RGB', img_size, raw, 'raw', 'BGR;15', 0, 1)
img.save('out.bmp')
