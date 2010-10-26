#!/usr/bin/env ruby

require 'enumerator'

module Enumerable
  def cons
    ary = []
    each_cons(2) {|a, b| ary << yield(a, b) }
    ary
  end

  def jolly?
    cons { |a, b| (a-b).abs }.sort.cons{|a, b|(a-b)==-1}.all?
  end
end

p [1, 4, 2, 3].jolly?
