#!/usr/bin/env python
# encoding: utf-8

f = open("/Users/yielding/Desktop/RouteHistory", "r")
s = f.read()
s = "123{ab{c}de}123{qwert}123"


beg = s.index("{")
end = s.rindex("}") + 1

have_start = True
match_count = 0
jsons = []

pos = beg
while pos < end:
    if s[pos] == '{':
        match_count += 1
        have_start = True
        if match_count == 1:
            beg = pos

    if s[pos] == '}':
        match_count -= 1

    if have_start and match_count == 0:
        jsons.append(s[beg:pos+1])
        have_start = False
    
    pos += 1

for json in jsons:
    print json
