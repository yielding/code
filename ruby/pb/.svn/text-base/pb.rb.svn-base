#!/usr/bin/env ruby -wKU

text = `pbpaste`
IO.popen('pbcopy', 'r+') do |pb|
  r = eval(text)
  puts "#result : #{r}"
  # pb.puts "result : #{r}"
end

