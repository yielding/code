class ByteBuffer2
  attr_reader :pos, :buffer

  def initialize(b, offset=0, length=-1)
    check_type(b)

    count = length == -1 ? b.size - offset : length
    if count > b.size - offset 
      raise IndexError 
    end

    if b.class == Array
      raise IndexError if offset + count > b.size
      @offset = 0
      @count  = count
      @buffer = b.slice(offset, count).pack("c*")
    else
      @offset = offset
      @count  = b.size - offset
      @buffer = b
    end

    @pos = offset
  end

  def bytes
    @buffer.bytes
  end

  def size
    @buffer.bytesize 
  end

  def reset
    @pos = @offset
    self
  end

  def flip
    reset
  end

  def pos=(pos)
    @pos = @offset + pos
  end

  def append(b, offset=0, length=-1)
    check_type(b)

    count = length == -1 ? b.size - offset : length
    if count > b.size - offset 
      raise IndexError 
    end

    @buffer += if b.class == Array
                 b.slice(offset, count).pack("c*")
               else 
                 b.slice(offset, count)
               end
    self
  end

  def get_string
    idx = @buffer.index("\x0", @pos)
    return "" if idx.nil?
    
    val  = @buffer.byteslice(@pos, @pos + idx)
    @pos = idx - @pos + 1
    val
  end

  def get_string_for(sz)
    raise IndexError.new("Too big size") if sz > size - @pos
    val = @buffer.byteslice(@pos, sz); @pos += sz
    val
  end

  def get_binary_for(size)
    get_string_for(size).bytes
  end

  def get_int1
    val = @buffer[@pos].ord; @pos += 1
    val
  end

  def get_int2_be
    arr = @buffer[@pos..@pos+1].bytes
    @pos += 2
    arr.pack("C*").unpack("n")[0]
  end

  def get_int2_le
    arr = @buffer[@pos..@pos+1].bytes
    @pos += 2
    arr.pack("C*").unpack("s")[0]
  end

  def get_uint2_be
    arr = @buffer[@pos..@pos+1].bytes
    @pos += 2
    arr.pack("C*").unpack("n")[0]
  end

  def get_uint2_le
    arr = @buffer[@pos..@pos+1].bytes
    @pos += 2
    arr.pack("C*").unpack("S")[0]
  end

  def get_int3_be
    arr = @buffer[@pos..@pos+2].bytes.unshift(0)
    @pos += 3
    arr.pack("C*").unpack("N")[0]
  end

  def get_int3_le
    arr = @buffer[@pos..@pos+2].bytes.reverse.unshift(0)
    @pos += 3
    arr.pack("C*").unpack("N")[0]
  end

  def get_int4_be
    arr = @buffer[@pos..@pos+3].bytes
    @pos += 4
    arr.pack("C*").unpack("N")[0]
  end

  def get_int4_le
    arr = @buffer[@pos..@pos+3].bytes
    @pos += 4
    arr.pack("C*").unpack("l")[0]
  end

  def get_uint4_be
    arr = @buffer[@pos..@pos+3].bytes
    @pos += 4
    arr.pack("C*").unpack("N")[0]
  end

  def get_uint4_le
    arr = @buffer[@pos..@pos+3].bytes
    @pos += 4
    arr.pack("C*").unpack("V")[0]
  end

  def get_int6_be
    arr = @buffer[@pos..@pos+5].bytes.unshift(0).unshift(0).reverse
    @pos += 6
    arr.pack("C*").unpack("Q")[0]
  end

  def get_int8_be
    arr = @buffer[@pos..@pos+7].bytes.reverse
    @pos += 8
    arr.pack("C*").unpack("Q")[0]
  end

  def get_int8_le
    arr = @buffer[@pos..@pos+7].bytes
    @pos += 8
    arr.pack("C*").unpack("Q")[0]
  end

  def get_varint_with_size
    viBytes  = @buffer[@pos..-1].bytes
    byte_no  = 0
    value    = 0
    complete = false

    while byte_no < 9 and byte_no < viBytes.size and not complete
      viByte = viBytes[byte_no]
      if (viByte & 0b10000000) == 0b10000000 and byte_no < 8
        value = (value << 7) | (viByte & 0b01111111)
      elsif (viByte & 0b10000000) == 0b10000000 and byte_no == 8
        value = (value << 8) | (viByte)
        complete = true
      else
        value = (value << 7) | (viByte & 0b01111111)
        complete = true
      end
      byte_no += 1
    end

    @pos += byte_no

    raise "No valid varint found" unless complete
    return value, byte_no
  end

  def get_varint
    value, _ = get_varint_with_size
    value
  end

  def check_type(b)
    raise "Invalid data type" unless [Array, String].include?(b.class)
  end
end
