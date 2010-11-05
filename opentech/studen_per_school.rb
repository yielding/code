#!/usr/bin/env ruby

if __FILE__ == $0
  Dir.glob("*.log") { |log| 
    File.open(log) { |f|
      time_count = Hash.new(0)
      f.readlines.each { |l| time_count[l.slice(1, 8)] += 1 }

      m = time_count.values.max
      
      puts "#{log}: #{time_count.index(m)} => #{m}"
    }
  }
end