# encoding: utf-8

require File.dirname(__FILE__) + '/test_helper.rb'

class TestPhoneNumber < Minitest::Test
  def setup
    @fname = "resources/number.pbk"
    @phone = Sky::PhoneNumber.new
    @phone.read(File.open(@fname))

    assert_equal(@phone.signature, 0x0303)
    assert_equal(@phone.magic, "number")
    assert_equal(@phone.skip, "\x00"*8)
  end

  def test_total_record_count
    div, mod = (File.size?(@fname) - 16).divmod(0x2E)
    assert_equal(mod, 0)
    assert_equal(div, @phone.record_count)
    rs = @phone.rids.size
    ns = @phone.numbers.size

    assert_equal(rs, @phone.count)
    assert_equal(ns, @phone.count)
  end

  def test_record_0
    rec = @phone.records[0]
    assert_equal(rec.rid, 1)
    assert_equal(rec.next_rid, 2)
    assert_equal(rec.phone_no, "0519970423")
  end

  def test_record_1
    rec = @phone.records[1]
    assert_equal(rec.phone_no, "01035950423")
  end
end
