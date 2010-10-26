def dal files
  Dir["*"].each do |dir| 
    if File.directory? dir
      Dir.chdir(dir) { dal files }
      return
    end
    files.each { |file| 
      p "#{Dir.pwd}/#{dir}" if Regexp.new(file).match(dir) 
    }
  end
end

if __FILE__ == $PROGRAM_NAME
  FILES = %w{\.doc$ \.xls$ }
  dal(FILES)
end
