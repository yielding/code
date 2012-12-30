# -*- coding:utf8 -*-

import clr

clr.AddReference("System.Windows.Forms")


from System.Windows.Forms import Application, Form 

class TestForm(Form):
  def __init__(self):
    self.Text = "Test"
    self.Width = 320
    self.Height = 200
    self.CenterToScreen()

Application.Run(TestForm())

# vi: set sw=4 sts=4 et
