require "byte_buffer"

describe ByteBuffer do
  context "ByteBuffer conversion" do
    before (:each) do
      @bytes = [*1..8]
      @bb    = ByteBuffer.new(@bytes)
    end

    it "should convert 1 bytes with range check" do
      @bytes.each { |byte| @bb.get_int1.should == byte }
      expect { @bb.get_int1 }.to raise_error(IndexError)

      @bb.pos = 7
      @bb.get_int1.should == 8
    end

    it "should read int2_be bytes correctly with range check" do
      actual = @bb.get_int2_be
      actual.should == (1<<8) + 2

      @bb.pos = 6
      @bb.get_int2_be.should == (7<<8) + 8

      @bb.pos = 7
      expect { @bb.get_int2_be }.to raise_error(IndexError)
    end

    it "should read int2_le bytes correctly with range check" do
      actual = @bb.get_int2_le
      actual.should == (2<<8) + 1

      @bb.pos = 7
      expect { @bb.get_int2_be }.to raise_error(IndexError)
    end

    it "should read uint2_be bytes correctly with range check" do
      actual = @bb.get_uint2_be
      actual.should == (1<<8) + 2

      @bb.pos = 7
      expect { @bb.get_uint2_be }.to raise_error(IndexError)
    end

    it "should read uint2_le bytes correctly with range check" do
      actual = @bb.get_uint2_le
      actual.should == (2<<8) + 1

      @bb.pos = 7
      expect { @bb.get_uint2_le }.to raise_error(IndexError)
    end

  end
end
