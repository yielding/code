#!/usr/bin/env ruby -KU
#encoding: utf-8

require "test/unit"
require "bindata"

class TestBinData < Test::Unit::TestCase
  def setup
    @be4 = BinData::Uint32be.new(1234)
  end

  def test_Uint32be
    converted = @be4.to_binary_s
    p converted
    assert_equal("\x00\x00\x04\xD2", converted)
  end
end

#ruby-1.9.3-p0 :002 > a = BinData::Uint32be.new(1234)
# => 1234 
#ruby-1.9.3-p0 :003 > a = BinData::Uint32le.new(1234)
# => 1234 
#ruby-1.9.3-p0 :004 > a = BinData::Uint32le.read("\000\000\004\322")
# => 3523477504 
#ruby-1.9.3-p0 :005 > a = BinData::Uint32be.read("\000\000\004\322")
