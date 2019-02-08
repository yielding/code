#!/usr/bin/env ruby

require_relative "hasher"
require_relative "redis_io"

path = "#{File.dirname(__FILE__)}/../data"
path = ARGV[0] if ARGV.size == 1 

#
# NOTICE
# '**' is difficult
# '**' does not mean parent dir.
# '**' means 'all the files recursively'

cache = HashSet.new
Dir.glob("#{path}/**/*.jpg").each { |file|  
  hs = Hasher.new
  hv = hs.file_hash(fname: file)
  if cache.member? hv
    puts "#{file} hash value is duplciated"
  else
    cache.add(hv)
  end
}
