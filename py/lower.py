#!/usr/local/bin/python2.0
import os
import glob
import re
import string
import sys
   
p = re.compile('(.*)[.](.*)$')

path = '*'
for i in glob.glob(path):
    m = p.match(i)
    if m:
       s1, s2 = m.group(1), m.group(2)
       oldname = s1 + '.' + s2
       s1 = s1.lower()
       s2 = s2.lower()
       newname  = s1+'.'+s2
       os.renames(oldname, newname)
       print s1, s2
