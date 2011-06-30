require "little_endian"

module Skype
  class CallHistory 
    include LittleEndian

    attr_reader :stream

    def initialize(path)
      @path = path
      @stream, @offset, @ok = nil, 0, false
      @recs = {}
    end

    def open
      if not @ok
        @stream = File.open(@path, "rb")
        @ok = (not @stream.nil?)
      end
      @ok
    end

    def open?; @ok end

    def offset
      @stream.pos 
    end

    def timestamp str
      an = str.unpack("C*").reverse
      a0 = an.shift
      an.reduce(a0) { |r, b| (r << 7) | (b & 0b01111111) }
    end

    def call_duration str
      index = -1
      arr = str.unpack("C*")
      arr.each_cons(2).with_index { |pair, i|
        if pair == [0xD1, 0x06] 
          index = i
          break 
        end
      }

      return 0 if index == -1

      bs = str.length - (index + 2)
      return str[-bs, bs].unpack("C*")[0]
    end

    def skip_blanks
      loop do
        c = @stream.getc
        if c != "\x00"
          @stream.ungetc c
          return
        end
      end
    end

    def get_header
      return @stream.read(32).unpack("a4VVa6a2a5a7")
    end

    def parse
      until @stream.eof?
        skip_blanks
        offset = @stream.pos
        head, size, rid, _, ts_, ts, _ = get_header
        raise unless head.eql?("l33l") or ts_.eql?("\xA1\x01")

        stamp = timestamp(ts)
        name  = get_string(0)        
        type  = get_string(1)        
        key_  = get_string(2);       raise unless key_.eql?("\xE4\x06")
        key   = get_string(0)        
        type  = get_string(1)        
        dir_  = get_string(2);       raise unless dir_.eql?("\xAD\x06")
        dir   = get_int1        

        left = get_string(offset + size + 8 - @stream.pos)
        duration = call_duration(left)
        rec = {
          :offset    => offset,
          :size      => size,  :session   => rid,
          :timestamp => stamp, :name      => name,
          :key       => key,   :direction => dir,
          :duration  => duration
        }

        @recs[rec[:offset]] = rec
      end
      p @recs
    end
  end
end
