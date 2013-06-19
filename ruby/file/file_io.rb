#!/usr/bin/env ruby2.0

require "test/unit"

class TestBinaryFile < Test::Unit::TestCase
  def setup
    @data  = "0123456789" * 2
    @fname = "database.bin"
  end

  def teardown
    File.delete(@fname) if File.exists?(@fname)
  end

  def test_write_with_seek
    f = File.new(@fname, "w+") 
    f.seek(0x100, 0)
    f.write [@data.length].pack("N")
    f.write @data
    f.close

    assert(File.exists?(@fname))
    assert_equal(0x100+20+4, File.size(@fname))
  end

  def test_pack4
    # "N" means Network byte order
    assert_equal("\0\0\0A", [65].pack("N"))
    assert_equal("A\0\0\0", [65].pack("L"))
  end
end
