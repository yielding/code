#!/usr/local/bin/python2.0
import Image

im = Image.open("lena.gif")
x,y = im.size
im = im.resize((x/2,y/2))
im.show()
im.save("smalllena.gif")
im.rotate(45).show()

