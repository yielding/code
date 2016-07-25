puts "access global variable"
puts "#{$ds.desc}\n"

puts "file system list"
$ds.file_systems.each { |name, fs| 
  puts " #{name}" 
}

fs = $ds.file_systems["hfs"]
p fs.name
 
puts "create new file"
f = File.new("/opt/local/include/iostream")

puts "inspect some properties"
puts "name  : #{f.name}"
puts "path  : #{f.path}"
puts "parent: #{f.parent}"
puts "size  : #{f.size}"
puts "delete: #{f.deleted?}"

p f.save_to('/Users/yielding/Desktop/result');
