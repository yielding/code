#!/usr/bin/env ruby
#
require "test/unit"

require_relative "sqlite"

class TestDBHeader < Test::Unit::TestCase
  def setup
  end

  def test_db_header
    header = File.binread("/Users/yielding/Desktop/KakaoTalk.db", 0x1000)
    dh     = SQLite::DatabaseHeader.new(header)
    assert_equal("SQLite format 3", dh.signature)
    assert_equal(0x1000, dh.page_size)
    assert_equal(0x40, dh.max_embedded_payload_fraction)
    assert_equal(0x20, dh.min_embedded_payload_fraction)
    assert_equal(0x20, dh.leaf_payload_fraction)
    assert_equal(0x830e, dh.file_change_counter)
    assert_equal(0x01eb, dh.db_file_size_in_pages)
    assert_equal(0x0000, dh.first_free_list_page_no)
    assert_equal(0x0000, dh.total_no_of_free_list_pages)
    assert_equal(0x0067, dh.schema_cookie)
    assert_equal(0x0003, dh.schema_format_no)
    assert_equal(0x0000, dh.default_cache_size)
    assert_equal(0x0029, dh.largest_root_btree_page_no)
    assert_equal(0x0001, dh.db_text_encoding)
    assert_equal(0x003a, dh.user_version)
    assert_equal(0x0000, dh.vaccum_mode)
    assert_equal(0x830e, dh.version_valid_for_no)
    assert_equal(0x002de21d, dh.sqlite_version_no)
  end

  def test_root_page
    header = File.binread("/Users/yielding/Desktop/KakaoTalk.db", 0x1000)
    ph = SQLite::PageHeader.new(header, 100)
  end

end

