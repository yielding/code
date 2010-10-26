#!/usr/bin/env ruby

def rm_r files
  Dir["*"].each do |dir| 
    if File.directory?(dir)
      Dir.chdir(dir) { rm_r(files) } 
    else 
      files.each {|file| p "#{Dir.pwd}/#{dir}" 
        if Regexp.new(file).match(dir) }
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  FILES = %w{\.ncb$ \.asp$ \.vcl$ \.vcb$ }
  Dir.chdir(ARGV[0])
  rm_r(FILES)
end
