#!/usr/bin/env ruby -wKU

class Mdbd
  def initialize path
    @path = path
    @file = nil    
  end

  def open
    @file = File.open(@path, "rb")
  end

  def open?
    not @file.nil?
  end

  def path
     @path 
  end

  def close
    File.close(@file) unless @file.nil?
  end

  def read_header
    @file.seek(0, IO::SEEK_SET)
    @file.read(6)
  end

end
