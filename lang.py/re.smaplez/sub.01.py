#!/usr/local/bin/python

import re;
"""
  patterm.sub(replace, source);

  즉 source string 에서 pattern을 매칭하는  expression을 replace로 바꾸어라
"""

p = re.compile( '(blue|white|red)');
print p.sub('colour', 'blue socks and red shoes');
print p.sub('colour', 'blue socks and red shoes', 1);

"""
result:
colour socks and colour shoes
colour socks and red shoes
"""


"""
empty matching exam
"""
p = re.compile('x*')
print p.sub('-', 'abxd')

"""
result:
-a-b--d-
"""
