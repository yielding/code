#!/usr/bin/env ruby 
require 'pp'

if __FILE__ == $0
  Dir.glob("*.log") { |log| 
    File.open(log)  { |f|
      time_count = Hash.new(0)
      f.readlines.each { |l| time_count[l.slice(1, 8)] += 1 }

      max_tr_time = time_count.values.max
      hot_spot =[]
      time_count.each_pair.map { |a, b| hot_spot.push(a) if b > 160 }
      hot_spot.sort.each {|e| puts e}

      puts "#{log}: #{time_count.index(max_tr_time)} => #{max_tr_time}"
      puts "bulk #{hot_spot.size}"
    }
  }
end
