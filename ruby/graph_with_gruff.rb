#!/usr/bin/env ruby
#encoding: utf-8

#require 'rubygems'
require 'gruff'

g = Gruff::Line.new
g.title = 'Gruff 시험'
g.data("사과", [1,2,3,4,4,3])
g.data("오렌지", [4,8,7,9,8,9])
g.data("수박", [2,3,1,5,6,8])
g.data("복숭아", [9,9,10,8,7,9])
g.labels = { 0=>'2006', 2=>'2007', 4=>'2008'}
g.font = "/System/Library/Fonts/AppleGothic.ttf"
g.write('gruff_test_result.png')
