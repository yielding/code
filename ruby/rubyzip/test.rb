#!/usr/bin/env ruby
require "zip"

forder = "/Users/yielding/code/cpp"
files = ['adl1.cpp', 'adl2.cpp']

res = "archive.zip"

Zip::File.open(res, Zip::File::CREATE) { |zf|
  files.each { |f| zf.add(f, forder + "/" + f) }

  zf.get_output_stream("myFile") { |os|
    os.write "my file contains just this"
  }
}
