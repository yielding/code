#!/usr/bin/env ruby

require "digest/sha1"
require "digest/md5"

File.class_eval do
  def self.hash_digest(filename, options = {})
    opts = {:buffer_length => 1024, :method => :sha1}.update(options)
    hash_func = (opts[:method].to_s == 'sha1') ? Digest::SHA1.new : Digest::MD5.new
    open(filename, "r") { |f|
      while !f.eof
        b = f.read
        hash_func.update(b)
      end
    }
    hash_func.hexdigest
  end
end

puts File.hash_digest(ARGV[0])
