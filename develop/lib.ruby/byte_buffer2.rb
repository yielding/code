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

class ByteBuffer2
  attr_reader :pos, :buffer, :count, :limit

  def initialize(b, offset=0, length=-1)
    check_type(b)

    count = length == -1 ? b.size - offset : length
    raise IndexError if count > b.size - offset 

    @begin  = offset
    @count  = count
    @limit  = @begin + count
    @pos    = offset
    @buffer = if b.class == Array
                b
              elsif b.class == String
                b.bytes
              else
                []
              end
  end

  def == (bb)
    return false if bb.class != ByteBuffer2

    pos == bb.pos and 
    count == bb.count and
    buffer == bb.buffer
  end

  def size
    @buffer.size 
  end

  def remained_size
    @limit - @pos
  end

  def empty?
    @pos >= @limit
  end

  def reset
    @pos = @begin
    self
  end

  def bytes
    @buffer.slice(@begin, @limit - @begin)
  end

  def flip
    reset
    self
  end

  def clear
    @buffer = []
    @pos    = 0
    @count  = 0
    @limit  = @begin
  end

  def pop(no)
    @buffer.pop(no)
    @count -= no
    @limit -= no
    self
  end

  def pos=(pos)
    @pos = @begin + pos
  end

  def append(b, offset=0, count_=-1)
    check_type(b)

    count = count_ == -1 ? b.size - offset : count_
    if not b.nil? and count > 0 
      to_append = if b.class == Array
                    b.slice(offset, count)
                  elsif b.class == String 
                    b.slice(offset, count).bytes
                  else
                    []
                  end

      @buffer += to_append
      @limit  += to_append.length
    end

    self
  end

  def get_string_length(delim)
    for index in @pos...@limit
      return index - @pos if @buffer[index] == delim
    end

    -1
  end

  def get_string
    len = get_string_length(0)
    return "" if len == -1

    val = @buffer.slice(@pos, len)
    @pos += len + 1
    val.pack("c*")
  end

  def get_string_for(sz)
    sz = size - @pos if sz > size - @pos

    val = @buffer.slice(@pos, sz); @pos += sz
    val.pack("c*")
  end

  def get_binary_for(size)
    get_string_for(size)
  end

  def get_bytes(size, at: -1)
    here = advance(at, size)
    get_string_for(here)
  end

  def get_byte(at: -1)
    here = advance(at, 1)
    @buffer[here].ord
  end

  def get_int1(at: -1)
    here = advance(at, 1)
    @buffer[here].ord
  end

  def get_int2_be(at: -1)
    here = advance(at, 2)
    arr = @buffer[here..here+1]
    arr.pack("C*").unpack("n")[0]
  end

  def get_int2_le(at: -1)
    here = advance(at, 2)
    arr = @buffer[here..here+1]
    arr.pack("C*").unpack("s")[0]
  end

  def get_uint2_be(at: -1)
    here = advance(at, 2)
    arr = @buffer[here..here+1]
    arr.pack("C*").unpack("n")[0]
  end

  def get_uint2_le(at: -1)
    here = advance(at, 2)
    arr = @buffer[here..here+1]
    arr.pack("C*").unpack("S")[0]
  end

  def get_int3_be(at: -1)
    here = advance(at, 3)
    arr = @buffer[here..here+2].unshift(0)
    arr.pack("C*").unpack("N")[0]
  end

  def get_int3_le(at: -1)
    here = advance(at, 3)
    arr = @buffer[here..here+2].reverse.unshift(0)
    arr.pack("C*").unpack("N")[0]
  end

  def get_int4_be(at: -1)
    here = advance(at, 4)
    arr = @buffer[here..here+3]
    arr.pack("C*").unpack("N")[0]
  end

  def get_int4_le(at: -1)
    here = advance(at, 4)
    arr = @buffer[here..here+3]
    arr.pack("C*").unpack("l")[0]
  end

  def get_uint4_be(at: -1)
    rbhere = advance(at, 4)
    arr = @buffer[here..here+3]
    arr.pack("C*").unpack("N")[0]
  end

  def get_uint4_le(at: -1)
    here = advance(at, 4)
    arr = @buffer[here..here+3]
    arr.pack("C*").unpack("V")[0]
  end

  def get_int6_be(at: -1)
    here = advance(at, 6)
    arr = @buffer[here..here+5].reverse.push(0, 0)
    arr.pack("C*").unpack("Q")[0]
  end

  def get_int8_be(at: -1)
    here = advance(at, 8)
    arr = @buffer[here..here+7].reverse
    arr.pack("C*").unpack("Q")[0]
  end

  def get_int8_le(at: -1)
    here = advance(at, 8)
    arr = @buffer[here..here+7]
    arr.pack("C*").unpack("Q")[0]
  end

  def get_double(at: -1)
    here = advance(at, 8)
    arr = @buffer[here..here+7]
    arr.pack("C*").unpack("G")[0]
  end

  def get_varint_with_size
    # viBytes  = @buffer[@pos..-1]    # 매우 의심스럽다.
    byte_no  = 0
    value    = 0
    complete = false

    # while byte_no < 9 and byte_no < viBytes.size and not complete
    while byte_no < 9 and @pos + byte_no < @buffer.size and not complete
      # viByte = viBytes[byte_no]
      viByte = @buffer[@pos + byte_no]
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

  def copy_range_from(from, size:)
    @buffer.slice(from, size)
  end

  def sub_buffer(from, size:)
    ByteBuffer2.new(@buffer.slice(from, size))
  end

  def check_type(b)
    raise "Invalid data type" unless [Array, String].include?(b.class)
  end

  def advance(at, dist)
    here = (at == -1) ? @pos : @begin + at
    @pos += dist if at == -1
    here
  end

  # def get_string_for(sz)
  #   raise IndexError.new("Too big size") if sz > size - @pos
  #   val = @buffer.slice(@pos, sz); @pos += sz
  #   val
  # end
end
