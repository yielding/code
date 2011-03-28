require 'pp'

puts $jukebox.unit
$jukebox.seek(3, 16) { |x| puts "#{x}% done" } 
p $jukebox

xx = CDJukebox.new(11223)
p xx.unit
p xx
