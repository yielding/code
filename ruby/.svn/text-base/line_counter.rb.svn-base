#!/usr/local/bin/ruby
# 1. how to comment multiple lines
# 2. better code design
# 3. study array, string (esp regex) more thoroughly

class LineCounter
  attr_reader :total
  
  def initialize(path)
    @path = path
    @total = 0
    unless File.exists?(@path)
      puts "wrong file path"
      exit
    end
  end
    
  def count(path, expr)
    paths = Dir.glob("*")
    paths.each do |path|
      if File.directory?(path)
        puts "aaaaa #{path} bbb"
        Dir.pwd(path) do |p| 
          count(p, expr)
        end
      else
        if path.match(expr)
          no = `wc -l #{path}`.split(' ')[0].to_i
          @total += no
          printf("%20s = %05s, %05s\n", path, no.to_s, @total.to_s)
        end
      end
    end
  end
  
  def get_count(expr)
    puts "aaa", @path
    count(@path, expr)
  end
end

lc = LineCounter.new("/Users/yielding/tmp")
lc.get_count("\.cpp$")

print lc.total
