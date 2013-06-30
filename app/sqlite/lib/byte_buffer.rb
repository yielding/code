class ByteBuffer
  BEG, CUR, LST = 0, 1, 2

  attr_accessor :buffer, :pos
  attr_reader   :limit

  def initialize(buffer, offset=0, count=-1)
    temp = (buffer.class == String) ? buffer.bytes : buffer
    @buffer = if count == -1
                 temp.slice(offset, buffer.length - offset)
              else
                 temp.slice(offset, count)
              end

    @pos = 0
  end

  def size
    @buffer.size
  end

  def reset
    @buffer.clear
    @pos = 0
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

  def set_int2_be(data)
    [data].pack("n").each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int2_le(data)
    [data].pack("v").each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int4_be(data)
    [data].pack("N").each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int4_le(data)
    [data].pack("V").each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int8_be(data)
    [data].pack("Q").reverse.each_byte { |byte| set_int1(byte) }
    self
  end

  def set_int8_le(data)
    [data].pack("Q").each_byte { |byte| set_int1(byte) }
    self
  end

  def get_ascii
    str = []
    while @buffer[@pos] != 0
      str << @buffer[@pos]
      @pos += 1
    end
    @pos += 1
    str.pack("C*")
  end

  def get_string(size)
    arr = Array.new(size)
    0.upto(size-1) { |i| arr[i] = @buffer[@pos+i] }
    @pos += size
    return arr.pack("C*")
  end

  def get_binary(size)
    arr = Array.new(size)
    0.upto(size-1) { |i| arr[i] = @buffer[@pos+i] }
    @pos += size
    arr
  end

  def get_int1
    bound_check(1)
    val = @buffer[@pos]; @pos += 1
    val
  end

  def get_uint2_be
    bound_check(2)

    arr = @buffer[@pos..@pos+1]
    @pos += 2
    res = arr.pack("C*").unpack("n")[0]
    res
  end

  def get_uint2_le
    bound_check(2)

    arr = @buffer[@pos..@pos+1]
    @pos += 2
    res = arr.pack("C*").unpack("S")[0]
    res
  end

  def get_int2_be
    bound_check(2)

    arr = @buffer[@pos..@pos+1]
    @pos += 2
    res = arr.pack("C*").unpack("n")[0]
    res
  end

  def get_int2_le
    bound_check(2)

    arr = @buffer[@pos..@pos+1]
    @pos += 2
    res = arr.pack("C*").unpack("s")[0]
    res
  end

  def get_uint4_be
    bound_check(4)
    arr = @buffer[@pos..@pos+3]
    @pos += 4
    res = arr.pack("C*").unpack("N")[0]
    res
  end

  def get_uint4_le
    bound_check(4)
    arr = @buffer[@pos..@pos+3]
    @pos += 4
    res = arr.pack("C*").unpack("V")[0]
    res
  end

  def get_int4_be
    arr = @buffer[@pos..@pos+3]
    @pos += 4
    res = arr.pack("C*").unpack("N")[0]
    res
  end

  def get_int4_le
    arr = @buffer[@pos..@pos+3]
    @pos += 4
    res = arr.pack("C*").unpack("l")[0]
    res
  end

  def get_int8_be
    arr = @buffer[@pos..@pos+7].reverse
    @pos += 8
    res = arr.pack("C*").unpack("Q")[0]
    res
  end

  def get_int8_le
    arr = @buffer[@pos..@pos+7]
    @pos += 8
    res = arr.pack("C*").unpack("Q")[0]
    res
  end

  def get_varint_with_size
    viBytes = @buffer[@pos..-1]
    byteNo  = 0
    value   = 0
    complete = false

    while byteNo < 9 and byteNo < viBytes.size and not complete
      viByte = viBytes[byteNo]
      if (viByte & 0b10000000) == 0b10000000 and byteNo < 8
        value = (value << 7) | (viByte & 0b01111111)
      elsif (viByte & 0b10000000) == 0b10000000 and byteNo == 8
        value = (value << 8) | (viByte)
        complete = true
      else
        value = (value << 7) | (viByte & 0b01111111)
        complete = true
      end
      byteNo += 1
    end

    @pos += byteNo

    raise "No valid varint found" unless complete
    return value, byteNo
  end

  def get_varint
    value, byteNo = get_varint_with_size
    return value
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

  private
  def bound_check(size)
    raise IndexError.new("Index exceeds buffer bound") if @pos > @buffer.length - size
  end

end
