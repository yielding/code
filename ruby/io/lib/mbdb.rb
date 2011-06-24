#!/usr/bin/env ruby -wKU

require "pp"

class Mbdb
  attr_reader :offset, :path
  attr_reader :mbdb

  def initialize(path)
    @path, @file, @ok = path, nil, false
    @mbdb   = {}
    @offset = 0
  end

  def open
    @file   = File.open(@path, "rb")
    @ok     = @file.read(6).unpack("C*") == [109, 98, 100, 98, 5, 0]
    @offset = 6
  end

  def open?
    @ok
  end

  def close
    File.close(@file) unless @file.nil?
  end

  def process_mbdb
    while @offset < @file.size
      fileinfo = {
        :offset => @offset,
        :domain => get_string,
        :path   => get_string,
        :linktg => get_string,
        :data_hash => get_string,
        :unknown1  => get_string,
        :mode      => get_int2,
        :unknown2  => get_int4,
        :unknown3  => get_int4,
        :user_id   => get_int4,
        :group_id  => get_int4,
        :mtime     => get_int4,
        :atime     => get_int4,
        :ctime     => get_int4,
        :file_len  => get_int8,
        :flag      => get_int1,
        :num_props => get_int1,
        :props     => {}
      }

      1.upto(fileinfo[:num_props]) { fileinfo[:props][get_string] = get_string }

      @mbdb[fileinfo[:offset]] = fileinfo
    end

  end

  def read_record
  end

  def get_string
    size = get_int2 
    return '' if size == 0xFFFF

    @offset += size
    return @file.read(size)
  end

  def get_int1; get_int(1) end
  def get_int2; get_int(2) end
  def get_int4; get_int(4) end
  def get_int8; get_int(8) end

  def get_int size
    @offset += size
    @file.read(size).each_byte.reduce(0) { |res, b| (res << 8) + b }
  end
end

if __FILE__ == $PROGRAM_NAME
  @db = Mbdb.new "/Users/yielding/code/ruby/io/data/5_Manifest.mbdb"
  @db.open
  @db.process_mbdb
  pp @db.mbdb
end
