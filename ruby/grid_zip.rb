#!/usr/bin/env ruby -wKU

a=[];3.times{ a<< gets.split}
p a
p (a+a.transpose).map{|l|l.inject(0){|s,i|s+i.to_i}}.max
