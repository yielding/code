# encoding: utf-8

require "pp"

class SQLiteCarver
  def initialize image_name
    raise "file not exists" unless File.exists?(image_name)
    @image_name = image_name
  end

  def size
    File.stat(@image_name).size 
  end

  def signature
    File.binread(@image_name, 6)
  end

end
