# coding: utf-8

import clr

clr.AddReference("System.Windows.Forms")
clr.AddReference("System.Drawing")

from System.Windows.Forms import *
from System.Drawing import *

class MainForm(Form):
    def __init__(self):
        self.SuspendLayout()
        button = Button()
        button.Text = u"Press it!"
        button.Location = Point(15, 15)
        button.Click += test
        self.Controls.Add(button)

        self.ResumeLayout(False)

def test(o, v):
    MessageBox.Show("Hello World")

form = MainForm()
Application.Run(form)
