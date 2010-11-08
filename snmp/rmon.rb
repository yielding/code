#!/usr/bin/env ruby

require 'test/unit'
require 'snmp'

include SNMP

class TestRMON < Test::Unit::TestCase
  def setup
    @host = "192.168.1.21"
    @port = 161
    @community = "adminmate"
    @snmp = Manager.new(:Host => @host, :Port => @port, :Community => @community )
  end

  def tearDown
    @snmp.close
  end

  def test_get_system
    results  = []
    requests = ["1.3.6.1.2.1.1.1.0", "1.3.6.1.2.1.1.2.0"]
    response = @snmp.get(requests)
    response.each_varbind do |vb| 
      #puts "name=#{vb.name} value = #{vb.value}"
      results << vb.name.to_s
    end
    assert_equal(requests, results)
  end

  def test_ifNumber
    results = []
    request = ["1.3.6.1.2.1.2.1.0"]
    response = @snmp.get(request)
    response.each_varbind do |vb| 
      #puts "name=#{vb.name} value = #{vb.value}"
      results << vb.value.to_s
    end
    assert_equal(results.length, 1)
    assert_equal(results[0].to_i, 26)
  end

  def test_on
  end

  def test_datasource
    request = ["1.3.6.1.2.1.2.2.1.2"]
    response = @snmp.walk(request)do |row| 
      row.each do |vb| 
        puts "name: #{vb.name} value: #{vb.value}"
      end
    end
  end

  # def test_utilization
  #   p_p    = 0.0
  #   p_o    = 0.0
  #   packet = 0.0
  #   octet  = 0.0

  #   toggle   = 1
  #   interval = 10
  #   loop do
  #     request = ["1.3.6.1.2.1.16.1.1.1.4.23", "1.3.6.1.2.1.16.1.1.1.5.23"]
  #     response = @snmp.get(request)
  #     response.each_varbind do |vb| 
  #       octet  = vb.value.to_f if toggle == 1
  #       packet = vb.value.to_f if toggle == 0
  #       toggle = 1 - toggle
  #     end
  #     
  #     utilization = ((packet-p_p) * 16  + (octet-p_o) * 0.8) / (interval * 10000.0)
  #     puts utilization
  #     p_p = packet
  #     p_o = octet

  #     sleep(interval)
  #   end
  # end

end
