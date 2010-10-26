#!/usr/bin/env ruby19

File.open('log.bin', 'rb:binary') { |f|
  count = 0
  f.bytes.each {|byte|
    puts if count % 4 == 0
    printf "%d ",  byte
    count += 1
  }
}
