#!/usr/bin/env ruby

p (5..10).reduce(1, :*)

arr = %w{ cat sheep bear }
p arr.reduce { |memo, word| 
  memo.length > word.length ? memo : word 
}
