#!/usr/bin/env ruby
# encoding: utf-8

a = "leech kamin 이창하공장"

if a =~/이창하([ㄱ-ㅎ|가-힣]+.*)/
  puts $1
end
