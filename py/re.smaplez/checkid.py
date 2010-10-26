#!/usr/bin/env python
import re

email   = "leech@tic.co.kr"
pattern = re.compile('[1-9a-zA-Z]{1}[1-9A-Za-z_]{1,7}@');
if pattern.match(email): print "ok" 
else: print "false"
