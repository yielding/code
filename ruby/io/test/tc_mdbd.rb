#!/usr/bin/env ruby

require "test/unit"

require "mdbd"

class TestMDBD < Test::Unit::TestCase
  def setup
    @file1 = File.expand_path(File.dirname(__FILE__)) + "/../data/5_Manifest.mbdb"
    @file1 = "/Users/yielding/code/ruby/io/data/5_Manifest.mbdb"
    @md  = Mdbd.new @file1
    @md.open 
  end

  def teardown
  end

  def test_open
    assert @md.open?
  end

  def test_path
    assert_equal(@file1, @md.path)
  end

  def test_read_header
    assert_equal [109, 98, 100, 98, 5, 0], @md.read_header.unpack("C*")
  end

end
