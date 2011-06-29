module LittleEndian
  def get_int1
    offset += 1; self.stream.read(1).unpack("v")[0]
  end

  def get_int2
    offset += 2; self.stream.read(2).unpack("v")[0]
  end

  def get_int4
    offset += 4; self.stream.read(4).unpack("V")[0]
  end

  def get_int8
    raise "Not implemented LittleEndian::get_int8"
  end
end
