# encoding: utf-8
require File.dirname(__FILE__) + '/test_helper.rb'

class TestPhoneNumber < Test::Unit::TestCase
  def setup
    @fname = "resources/number.pbk"
    @io    = File.open(@fname)
    @phone = Sky::PhoneNumber.new
    @phone.read(@io)

    assert_equal(@phone.signature, 0x0303)
    assert_equal(@phone.magic, "number")
    assert_equal(@phone.skip, "\x00"*8)
  end

  def test_file_exists
    assert(@io, "number.pbk not exists")
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
    assert_equal(rec.phone_no.strip, "0519970423")
  end

 def test_record_1
   rec = @phone.records[1]
   assert_equal(rec.phone_no.strip, "01035950423")
 end

end
