#!/usr/bin/env ruby19

#[*1..4].permutation.each { |e| p e }

1.upto(4) { |i| 
  [*1..i].permutation.each { |e| p e } 
}
