#!/usr/bin/env ruby

if __FILE__ == $0
  if ARGV.size < 1
    puts "USAGE: low_list.rb filename" 
    exit
  end
  
  File.open(ARGV[0]) do |f|
    total = 0
    hb = 0
    f.readlines.each { |l|
      hb += 1 if l.length <= 35
      total += 1  
    }
    p = hb.to_f / total
    puts "total: #{total} hb: #{hb}, percent: #{p}"
  end
end
