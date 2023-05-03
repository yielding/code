#!/usr/bin/env ruby
#encoding: utf-8

def search_block(block, key)
  pos_list = []
  pos = block.index(key)
  while pos
    yield pos if block_given?
    pos_list << pos
    pos = block.index(key, pos+1)
  end

  pos_list
end

def search_file(s, beg, end_, key)
  size = end_ - beg + 1
  blks = (size + 1023) / 1024
  offsets = []
  blks.times { |n|
    s.seek(beg + n * 1024)
    block = s.read(1024 + key.size - 1)
    roffsets = search_block(block, key)
    offsets += roffsets.map { |e| e + beg + n * 1024 }
  }

  offsets
end

if __FILE__ == $PROGRAM_NAME
  path = "data0.bin"
  f    = File.open(path, "rb")
  size = File.size(path)
  p search_file(f, 0, size-1, "monday") 
end
