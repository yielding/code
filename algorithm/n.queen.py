#!/usr/bin/env python
# encoding: utf-8

from itertools import permutations

n = 4                                    # 8
cols = range(n)
for v in permutations(cols):
  #print v
  l1 = len(set(v[i] + i for i in cols))  # 배열의 각 엔트리(row)에 칼럼값을 더하거나 빼면
  l2 = len(set(v[i] - i for i in cols))  # 같은 대각선에 있는 놈들은 같은 값을 가진다.
                                         # 기울기의 절대값이 1인것이 아이디어
  if (l1 == l2 == n):
      print "%d, [%s]" % (n, str(v))
