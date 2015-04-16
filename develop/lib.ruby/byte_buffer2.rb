=begin rdoc

  The variable-length integer encoding is as follows:

  KEY:
          A = 0xxxxxxx    7 bits of data and one flag bit
          B = 1xxxxxxx    7 bits of data and one flag bit
          C = xxxxxxxx    8 bits of data

   7 bits - A
  14 bits - BA
  21 bits - BBA
  28 bits - BBBA
  35 bits - BBBBA
  42 bits - BBBBBA
  49 bits - BBBBBBA
  56 bits - BBBBBBBA
  64 bits - BBBBBBBBC

  Write a 64-bit variable-length integer to memory starting at p[0].
  The length of data write will be between 1 and 9 bytes.  The number
  of bytes written is returned.

  A variable-length integer consists of the lower 7 bits of each byte
  for all bytes that have the 8th bit set and one byte with the 8th
  bit clear.  Except, if we get to the 9th byte, it stores the full
  8 bits and is the last byte.

=end

class Numeric
  def get_varint

    val = self
    res, buf = [0] * 10, [0] * 10
    #
    # we treat this specially because we have 'C' type at the end
    #
    if (val & (0xff000000 << 32)) != 0
      res[8] = val & 0xff
      val >>= 8
      7.downto(0) do |i| 
        res[i] = (val & 0x7f) | 0x80 
        val >>= 7 
      end

      return res.take(9).reverse
    end

    n = 0
    until val == 0
      buf[n], n = (val & 0x7f) | 0x80, n+1 
      val >>= 7
    end

    buf[0] &= 0x7f
    return buf.take(n).reverse
  end
end

#
#
#
class ByteBuffer2
  attr_reader :pos, :buffer, :count

  def initialize(b, offset=0, length=-1)
    check_type(b)

    count = length == -1 ? b.size - offset : length
    raise IndexError if count > b.size - offset 

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

  def remained_size
    size - @pos
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

  def get_bytes(size)
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

  def get_double
    arr = @buffer[@pos..@pos+7].bytes
    @pos += 8
    arr.pack("C*").unpack("G")[0]
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
