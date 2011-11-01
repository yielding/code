#!/usr/bin/env ruby

File.open('log.bin', 'rb:binary') { |f|
  count = 0
  f.bytes.each { |byte|
    puts if count % 4 == 0
    printf "%-4d", byte
    count += 1
  }
}
