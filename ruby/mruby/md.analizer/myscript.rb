puts "#{$ds.desc}\n"

puts "file system list"
$ds.file_systems.each { |name, _| puts " #{name}" }

a = $ds.file_systems["hfs"]
p a.name

f = File.new("/opt/local/include/iostream")

p f.name
p f.path
p f.parent
p f.size
p f.deleted

p f.save_to('/Users/yielding/Desktop/result');
