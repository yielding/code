#!/usr/bin/env ruby
# encoding: utf-8

require "bindata"

module Sky
  class Book < BinData::Record
    endian :little
    uint16 :signature
    string :magic, :read_length => 4
    string :skip,  :read_length => 10
    array  :records, :initial_length => 1200 do
      uint16 :s0
      uint16 :self_index       
      uint16 :group_index
      uint16 :phone_no_index
      uint16 :aniversary_index
      uint16 :email_index
      string :name, :read_length => 16
      string :s1,   :read_length => 0x214 - 12 - 16
    end

    def names
      res = []
      1.upto(1200) do |i|
        n = records[i].name.encode("utf-8", "euc-kr")
        res << n.strip
      end
      res
    end

    def phone_no_indices
      res = []
    end

    def 

  end
end

require "test/unit"

class TestBook < Test::Unit::TestCase
  def setup
    @fname = "book.pbk"
    @io   = File.open(@fname)
    @book = Sky::Book.new
    @book.read(@io)

    assert_equal(@book.signature, 0x0303)
    assert_equal(@book.magic, "book")
    assert_equal(@book.skip, "\x00"*10)
  end

  def test_open
    assert(@io, "File not exists")
  end

  def test_rec_count
   p Flie.size?(@fname)
  end

  def test_records_0
    rec = @book.records[0]
    assert_equal(rec.s0, 1)
    assert_equal(rec.self_index, 2)
    assert_equal(rec.group_index, 1)
    assert_equal(rec.phone_no_index, 1)
    assert_equal(rec.aniversary_index, 0)
    assert_equal(rec.email_index, 0)
    p rec.name.strip.encode("utf-8", "euc-kr")
  end
end
