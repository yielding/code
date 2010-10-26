#!/usr/bin/env ruby

require 'test/unit'

class TestSubstitute < Test::Unit::TestCase
  def test_count
     `find . -name '*.c' | xargs`.split.each do |file|
     contents = IO.read(file).gsub(/mainn/, 'main')
     base = File.basename(file, ".c")
     ext  = File.extname(file)
     new_name = "#{base}.new#{ext}"
     File.new(new_name, "w").write(contents)
    end
  end
  
end
