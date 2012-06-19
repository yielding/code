$jb = CDJukeBox.new(3); 
puts "unit id = #{$jb.unit}"

arr = $jb.dvd_list
p arr
p arr[0].name
#arr.each { |dvd| p dvd.name }

$d = DVD.new("leech")
puts $d.name

$d.name = "kamin"
puts $d.name

# puts "Block test with extended object !"
# 
# class CDJukeBox
#   def count
#    10 
#   end
# end
# 
# $jb = CDJukeBox.new(1); 
# puts "unit id = #{$jb.unit}"
# 
# $jb.unit = 2
# puts "unit id = #{$jb.unit}"
# 
# arr = []
# $jb.seek(3, 4) { |percent| arr << percent }
# 
# p arr.to_s
# p "count is #{$jb.count}"
# p "average seek time #{$jb.avg_seek_time}"
