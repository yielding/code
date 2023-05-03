#!/usr/bin/env ruby

def fast_sum(n)
  return 1 if n == 1
  return fast_sum(n-1) + n if n.odd?
  return 2 * fast_sum(n/2) + (n/2) * (n/2)
end
