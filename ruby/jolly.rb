#!/usr/bin/env ruby
# encoding: utf-8

require 'enumerator'

module Enumerable
  def cons
    ary = []
    each_cons(2) { |a, b| ary << yield(a, b) }
    ary
  end

  def jolly?
    cons { |a, b| (a-b).abs }.sort.cons{|a, b|(a-b)==-1}.all?
  end
end

p [1, 4, 2, 3].jolly?
p [10, 5, 1, 4, 6, 12, 19, 27, 26].jolly?
