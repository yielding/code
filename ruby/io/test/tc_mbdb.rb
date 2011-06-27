#!/usr/bin/env ruby

require "test/unit"

require "mbdb"

class TestMbdb < Test::Unit::TestCase
  def setup
    @file1 = File.expand_path(File.dirname(__FILE__)) + "/../data/5_Manifest.mbdb"
    @file1 = "/Users/yielding/code/ruby/io/data/5_Manifest.mbdb"
    @db = Mbdb.new @file1
    @db.open 
  end

  def teardown
  end

  def test_open
    assert @db.open?
  end

  def test_path
    assert_equal(@file1, @db.path)
  end

  def test_read_header
    assert_equal 6, @db.offset
  end

  def test_process
    @db.process_mbdb
  end

end
