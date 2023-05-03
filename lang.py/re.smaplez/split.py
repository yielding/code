#!/usr/local/bin/python

import re;
"""
pattern.split(string)  : string을 명시된 pattern에 맞게 잘라서 sub-string list를 리턴

\W == [^a-zA-Z0-9] 즉 알파뉴메릭이 아닌 하나의 문자
"""

p1 = re.compile(r'\W+')
p2 = re.compile(r'(\W+)')

print p1.split('This... is a test.')
print p2.split('This... is a test.')
"""
['This', 'is', 'a', 'test', '']
['This', '... ', 'is', ' ', 'a', ' ', 'test', '.', '']
"""

print re.split('[\W]+',   'Words, words, words.')
print re.split('([\W]+)', 'Words, words, words.')
"""
['Words', 'words', 'words', '']
['Words', ', ', 'words', ', ', 'words', '.', '']
"""
