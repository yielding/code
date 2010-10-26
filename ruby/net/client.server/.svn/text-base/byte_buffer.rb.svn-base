class ByteBuffer
  BEG, CUR, LST = 0, 1, 2

  attr_accessor :buffer
  attr_reader   :pos, :size, :limit

  def initialize(size=0)
    @buffer = Array.new(size); @pos = 0
    @size = size
  end

  def from_packet(string)
    @buffer = string.unpack("C*").to_a
    @size = buffer.size;
    @pos = 0
  end

  def from_array(buf)
    @buffer = Array.new(buf)
    @size = @buffer.size
    @pos = 0
  end

  def size
    @buffer.size
  end

  def clear
    @buffer.clear
  end

  def to_packet
    @buffer.pack("C*")
  end

  def set_string(data)
    data.each_byte { |byte| set_int1(byte) }
    set_int1(0)
  end

  def set_string_without_null(data)
    data.each_byte { |byte| set_int1(byte) }
  end

  def set_binary(data)
    raise "data is not binary" if data.class != Array
    data.each { |byte| @buffer[@pos] = byte; @pos += 1 }
    self
  end

  def set_int1(data)
    @buffer[@pos] = data
    @pos += 1
    self
  end

  def set_int2(data)
    [data].pack("n").each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int4(data)
    [data].pack("N").each_byte { |byte| set_int1(byte) }
    self
  end

  def get_string
    str = []
    while @buffer[@pos] != 0
      str << @buffer[@pos]
      @pos += 1
    end
    @pos += 1
    str.pack("C*")
  end

  def get_binary(size)
    arr = Array.new(size)
    0.upto(size-1) {|i| arr[i] = @buffer[@pos+i] }
    @pos += size
    arr
  end

  def get_int1
    val = @buffer[@pos]; @pos += 1
    val
  end

  def get_int2
    arr = @buffer.values_at(@pos, @pos+1)
    @pos += 2
    res = arr.pack("C*").unpack("n")[0]
    res
  end

  def get_int4
    arr = @buffer.values_at(@pos, @pos+1, @pos+2, @pos+3)
    @pos += 4
    res = arr.pack("C*").unpack("N")[0]
    res
  end

  def flip
    @buffer.compact!
    @pos = 0
  end

  def write path
    f = File.new(path, "w+b")
    f.write(@buffer.pack("C*"))
  end

  def peek1_at(offset, start=CUR)
    @buffer[offset]
  end

  def xor_upto(count, from=0)
    return 0 if count <= 0

    offset = from
    arr    = @buffer[offset...offset+count]
    return arr.inject { |xored, v| xored ^= v }
  end

end
