#!/usr/bin/env python
from PIL import Image

file = open('tower.rads', 'rb')
raw  = file.read()
file.close()

img_size = (240, 192)
img = Image.fromstring('RGB', img_size, raw, 'raw', 'BGR;16', 0, 1)
img.save('tower.bmp')
img.show()
