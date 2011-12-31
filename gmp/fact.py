import time

def factorial(N):
  """docstring for factorial"""
  fac = 1
  for i in xrange(1, N+1):
    fac *= i

  return fac

n = 200000
start = time.time()
factN = factorial(n)

print "runtime is %s" % (time.time() - start)
