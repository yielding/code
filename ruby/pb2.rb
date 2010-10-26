#!/usr/bin/env ruby

$VERBOSE = true
begin
   file_name = ARGV.shift or raise 'You need to supply a file name'
   IO.popen('pbcopy', 'r+') do |clipboard|
     clipboard.puts "% cat #{file_name}"

     file_src = nil
     File.open(file_name) do |source_code|
       file_src = source_code.read
     end
     clipboard.puts file_src

     clipboard.puts ''

     ruby_res = nil
     IO.popen('/usr/bin/ruby 2>&1', 'r+') do |ruby|
       ruby.puts 'STDOUT.sync = true'
       ruby.puts %Q{
       begin
         old_verbose = $VERBOSE
         $VERBOSE = nil
         ARGV = #{ARGV.inspect}
       ensure
         $VERBOSE = old_verbose
       end
       }
       ruby.puts file_src
       ruby.close_write
       ruby_res = ruby.read
puts ruby_res
     end
     clipboard.puts "% ruby #{file_name}"
     clipboard.puts ruby_res
#puts "% ruby #{file_name}"
#puts ruby_res
   end
rescue => ex
   STDERR.puts "#{File.basename($0)}: error: #{ex.message}"
end
