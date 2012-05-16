#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "../lib/book.rb"

class TestBook < Test::Unit::TestCase
  def setup
    @fname = "resources/book.pbk"
    @io    = File.open(@fname)
    @book  = Sky::Book.new
    @book.read(@io)

    assert_equal(@book.signature, 0x0303)
    assert_equal(@book.magic, "book")
    assert_equal(@book.skip, "\x00"*10)
  end

  def test_file_exists
    assert(@io, "File not exists")
  end

  def test_total_record_count
    div, mod = (File.size?(@fname) - 16).divmod(0x214)
    assert_equal(mod, 0)
    assert_equal(div, 1200)
    assert_equal(div, @book.record_count)
  end

  def test_actual_record_size
    actual_size = @book.count
    assert_equal(actual_size, 388)
  end

  def test_self_indices
    assert_equal(@book.self_indices.size, @book.count)
  end

  def test_records_0
    rec = @book.records[0]
    assert_equal(rec.rid, 1)
    assert_equal(rec.self_index, 2)
    assert_equal(rec.group_index, 1)
    assert_equal(rec.phone_no_index, 1)
    assert_equal(rec.aniversary_index, 0)
    assert_equal(rec.email_index, 0)
    name = rec.name.strip.encode("utf-8", "euc-kr")
    assert_equal("처음처럼", name)
  end
end
