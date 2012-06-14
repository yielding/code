puts "block test with extended object"

$jb = CDJukebox.new(1); 
puts "unit id = #{$jb.unit}"

$jb.unit = 2
puts "unit id = #{$jb.unit}"

$jb.seek(3, 4) { |percent| p percent }

