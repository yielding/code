#!/usr/bin/env ruby
# encoding: utf-8

p (5..10).reduce(1, :*)

arr = %w{ cat sheep bear }
p arr.reduce { |memo, word| 
  p "[#{memo}, #{word}]"
  memo.length > word.length ? memo : word 
}
