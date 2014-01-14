#!/usr/bin/env ruby
# encoding: utf-8

class Array
  def permutations
    return [self] if size < 2
    perm = []
    each {|e| (self - [e]).permutations.each { |p| perm << ([e] +p) } }
    perm
  end
end


p [*1..4].permutations
