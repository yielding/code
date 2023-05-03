#!/usr/bin/env ruby

require 'test/unit'
require 'snmp'
require 'pp'

include SNMP

class TestRMON < Test::Unit::TestCase
  def setup
    #@host = "192.168.10.11"
    @host = "192.168.10.128"
    @port = 161
    @community = "sol123"
    @snmp = Manager.new(:Host => @host, :Version=>:SNMPv1, :Port => @port, :Community => @community)
    pp @snmp
  end

  def teardown
    @snmp.close
  end

  def test_cpu100
    puts "start cpu100"
    #request = ["1.3.6.1.4.1.2021.2.1.3"]
    request = ["1.3.6.1.2.1.25.3.3.1.2"]
    response = @snmp.walk(request) do |row| 
      row.each do |vb| 
        puts "name: #{vb.name} value: #{vb.value.to_f}"
      end
    end
  end
end
