#!/usr/bin/env ruby -wKU

infile = ARGV[0] || "#{Dir.pwd}/Maptiles.sql"
dir    = "#{Dir.pwd}/maptiles-output"
Dir.mkdir(dir, 0755) if not Dir.exists?(dir)

File.open(infile) { |file|
  file.readlines.each { |line| 
    next unless line =~ /^INSERT/
    fields = line.split(/\(|\)/)[1]
    zoom, x, y, flags, length, image = fields.split(/\,/)
    puts "#{zoom}, #{x}, #{y}, #{flags}, #{length}"
    # data = image.sub(/^X'/, '').sub(/'$/, '')
    data = image.sub(/^X'|'$/, '')
    next if image.length < 128
    
    filename = "#{dir}/#{x},#{y}\@#{zoom}.png"
    File.open(filename, "w") { |f2|
      0.step(image.length, 2) { |i| f2 << "0x#{data[i, 2]}".hex.chr }
    }
  }
}