#!/usr/bin/env ruby

require 'rubygems'
require 'active_record'
require 'test/unit'
require 'pp'

ActiveRecord::Base.establish_connection(
 :adapter  => 'postgresql',
 :database => 'o2',
 :host => 'pg.solbrain.co.kr',
 :username => 'postgres',
 :password => 'sol123')

class Sensor < ActiveRecord::Base
  belongs_to :device
end

class Device < ActiveRecord::Base
  has_many :sensors
end

class TestPG < Test::Unit::TestCase
  def setup
    @dev = Device.new(
                               :device_name => "db server",
                               :device_desc => "sol's db server",
                               :device_hostname => "192....128",
                               :snmp_read_community => "yk babo",
                               :snmp_write_community => "yk babo",
                               :snmp_version => 1,
                               :snmp_timeout => 30,
                               :snmp_retry => 2
                               )
    @dev.save      
    @s1 = Sensor.new(:device_id => @dev.id,
                              :svc_plugin_name => "unix_sensor.so",
                              :sensor_name => "unix_sensor",
                              :sensor_desc => "unix sensor by sol",
                              :monitoring => 1,
                              :msc_interval => 30)
    @s1.save
    @s2 = Sensor.new(:device_id => @dev.id,
                              :svc_plugin_name => "win32_sensor.so",
                              :sensor_name => "windows_sensor",
                              :sensor_desc => "windows sensor by sol",
                              :monitoring => 0,
                              :msc_interval => 30)
    @s2.save
  end
  
  def teardown
    @s1.destroy
    @s2.destroy
    @dev.destroy
  end
  
  def test_count
    assert_equal(Device.count, 1)
    assert_equal(Sensor.count, 2)
  end
  
  def test_sensor_names
    arr = []
    Sensor.find(:all, :select=>"sensor_name").each { |s|
      arr << s.attributes["sensor_name"]
    }
    assert_equal(arr, ["unix_sensor", "windows_sensor" ])
  end
end


#Rfsignal.find(:all).each { |s| p s.edu_code }

#Dept.find(:all, :select => "dname").each { |s| puts s.dname }
