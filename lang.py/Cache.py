#!/usr/bin/env python

class Cacheable:
  _cache = {}
  @classmethod
  def create_cache(cls, value):
    if not cls._cache.has_key(value):
      print "cache miss"
      result = cls(value)
      cls._cache[value] = result
    else:
      print "cache hit"
      result = cls._cache[value]
      return result

class X(Cacheable):
  def __init__(self, value):
    self.value = value

x = X.create_cache("test")
x = X.create_cache("test2")
x = X.create_cache("test")
