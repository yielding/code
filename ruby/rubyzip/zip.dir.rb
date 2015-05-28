#!/usr/bin/env ruby
require "zip"

dir  = "/Users/yielding/code/ruby"
dest = "/Users/yielding/Desktop/ruby.gz"

options = { "directories-recursively" => true }
Zip::File.open(dest, Zip::File::CREATE) { |zf|
  

}
