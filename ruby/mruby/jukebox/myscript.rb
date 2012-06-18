puts "Block test with extended object !"

class CDJukebox
  def count
   10 
  end
end

$jb = CDJukebox.new(1); 
puts "unit id = #{$jb.unit}"

$jb.unit = 2
puts "unit id = #{$jb.unit}"

arr = []
$jb.seek(3, 4) { |percent| arr << percent }

p arr.to_s
p "count is #{$jb.count}"
<<<<<<< HEAD
=======
p "average seek time #{$jb.avg_seek_time}"
>>>>>>> 85cc03678e9cfc1d843992335ab29b9b560d373b
