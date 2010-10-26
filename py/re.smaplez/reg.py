#!/usr/local/bin/python2.0

import os
import glob
import re

os.chdir('/usr/bin')

p = re.compile('.*in.*')

for i in glob.glob('*'):
    m = p.match(i)
    if m: print m.group()
