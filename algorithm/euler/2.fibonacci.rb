#!/usr/bin/env ruby -wKU

## no. 2
# def fib_upto(n)
#   i0, i1 = 1, 1
#   while i0 <= n
#     yield i0
#     i0, i1 = i1, i0 + i1
#   end
# end
# 
# arr = []
# fib_upto(4000000) { |f| arr << f }
# arr.shift # duplicated 1 is removed
# 
# sum = 0
# arr.each { |a| sum += a if (a % 2 == 0) } 
# p sum # 4613732

# no. 2.1
def fib_upto_1(n)
  i0, i1 = 1, 1
  while i0 <= n
    yield i0 if i0 % 2 == 0
    i0, i1 = i1, i0 + i1
  end
end

# no.2.2
#
def fib_upto_2(n)
  sum = 0
  a = 1
  b = 1
  c = a + b
  while c <= n
    sum += c
    a = b + c
    b = c + a
    c = a + b
  end
  sum
end

no  = 40000000000000000000000000000000000000000000000
now = Time.now
arr = []
fib_upto_1(no) { |f| arr << f }
res = arr.inject(:+)
printf "%d, %d\n", Time.now - now, res

now = Time.now
res = fib_upto_2(no)
printf "%d, %d\n", Time.now - now, res
