module BigEndian
  def get_int1; get_int(1) end

  def get_int2; get_int(2) end

  def get_int4; get_int(4) end

  def get_int8; get_int(8) end

  private
  def get_int size
    @stream.read(size).each_byte.reduce(0) { |res, b| (res << 8) + b }
  end
end
