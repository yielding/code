#!/usr/bin/env ruby

require 'rx'

Rx::Observable.range(1, 10)
  .select    { |no| no.even? }
  .sum 
  .subscribe { |s| puts "The sum of the even from 1 ~ 10 is: #{s}" }
