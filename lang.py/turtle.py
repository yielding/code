#!/usr/bin/env python

from turtle import Turtle
from random import random, uniform

t = Turtle()
t.shape("turtle")
t.speed(0)

colors = ["red", "orange", "yellow", "green", "blue", "purple"]

for i in range(30):
   radius = int(uniform(50, 100))
   x = int(uniform(-500, 500))
   y = int(uniform(-500, 500))
   c = int(uniform(0, len(colors)))
   t.color(colors[c])

   t.penup()
   t.goto(x, y)
   t.pendown()
   t.begin_fill()
   t.circle(radius)
   t.end_fill()

t.screen.mainloop()


## 2 functions for drawing
#def init_grapics():
#    t = Turtle()
#    t.shape("turtle")
#    t.speed(0)
#    return t
#
#def draw_circle(t, x, y, radius, color):
#    t.penup()
#    t.goto(x, y)
#    t.pendown()
#    t.color(color)
#    t.begin_fill()
#    t.circle(radius)
#    t.end_fill()
#
#t = init_grapics()
#
#for i in range(30):
#    radius = int(uniform(50, 100))
#    x = int(uniform(-500, 500))
#    y = int(uniform(-500, 500))
#    c = int(uniform(0, len(colors)))
#    draw_circle(t, x, y, radius, colors[c])
#
#t.screen.mainloop()
