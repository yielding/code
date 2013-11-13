data = `pbpaste`

IO.popen('pbcopy', 'r+') do |pb|
  result = "[ 0x#{data.split.join(", 0x")} ]"
  puts "#result = #{result}"
  pb.puts "result = #{result}"
end
