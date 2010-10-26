# wookay_ext
# 2006  wookay

class String
  # 2006.4.5 wookay
  def split_to_hash
    uno, dos = ',', '='
    h = {}
    split(uno).map{|x|x.split dos}.each{|a,b| h.store a, b}
    h
  end
end

class Hash
  # 2006.4.5 wookay
  def getKey name
    keys.all?{|x|x.class==Symbol} ? name.to_sym : name
  end
  def method_missing(m, *args)
    name = m.to_s
    if name[-1] == ?=
      store getKey(name[0..-2]), args[0]
    else
      fetch getKey(name)
    end
  end
end

require 'enumerator'

module Enumerable
  # 2006.4.4 wookay
  def with_index
    ary = []
    each_with_index { |x,idx| 
      ary << yield(x,idx) 
    }
    ary
  end

  # 2006.4.3 wookay
  def cons
    ary = []
    each_cons(2) {|a,b| ary << yield(a,b) }
    ary
  end
  
  # 2006.3.31 wookay
  def sum
    inject{|s,v| s+=v}
  end
  
  # 2006.3.31 wookay
  def digits
    to_s.scan(/./).map{|x|x.to_i}
  end
end

class Regexp
  # 2006.3.24 wookay
  def first data
    m = match data
    m[1] if m
  end
end

if __FILE__== $0
  p 'hello'=='a=hello,b=,c=0'.split_to_hash.a
  p (0..9).sum*2==(0..9).map.with_index{|x,idx|x+idx}.sum
  p (0..9).sum=='0123456789'.digits.sum
  p 9==(1..10).cons{|a,b|b-a}.sum
  p 'world'==/.*(w.*)/.first("hello world")
end
