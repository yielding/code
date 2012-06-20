# coding: utf-8

class CDJukeBox
  def count
   10 
  end
end

$jb = CDJukeBox.new(1); 
puts "unit id = #{$jb.unit}"

$jb.unit = 2
puts "unit id = #{$jb.unit}"

arr = []
$jb.seek(3, 4) { |percent| arr << percent }

p arr.to_s
p "count is #{$jb.count}"
p "average seek time #{$jb.avg_seek_time}"

$jb.dvd_list.each  { |dvd| p dvd.name }
$jb.user_list.each { |k, v| p "name: #{k}, value: #{v}" }

$d = DVD.new("leech"); puts $d.name
$d.name = "kamin";     puts $d.name

box = $mstore.create_jukebox(10)
puts box.unit
