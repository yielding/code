#!/usr/bin/env python

import pdb

def area(boxes):
    s = set([])
    for x1, y1, x2, y2 in boxes:
        for x in xrange(x1, x2):
            for y in xrange(y1, y2):
                s.add((x, y))

    return len(s)

boxes = [[0, 0, 200, 200]] * 4
print area(boxes)

boxes = [[1, 2, 4, 4], [2, 3, 5, 7], 
         [3, 1, 6, 5], [7, 3, 8, 6]]
print area(boxes)
