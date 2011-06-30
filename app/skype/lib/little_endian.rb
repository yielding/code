module LittleEndian
  def get_int1
    @stream.read(1).unpack("C")[0]
  end

  def get_int2
    @stream.read(2).unpack("v")[0]
  end

  def get_int4
    @stream.read(4).unpack("V")[0]
  end

  def get_int8
    raise "Not implemented LittleEndian::get_int8"
  end

  def get_int size
    res = 0
    arr = @stream.read(size).unpack("C")
    arr.each_with_index { |b, index|
      res = (b << 8*index) | res
    }
    res
  end

  def get_string size
    return @stream.read(size) if size > 0

    str = ""
    loop do
      c = @stream.getc
      break if c == "\x00"
      str << c
    end
    str
  end

end
