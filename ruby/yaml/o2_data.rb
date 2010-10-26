#!/usr/bin/env ruby

require "yaml"
require "pp"

data = {
  :devices => { 
    :dev1 => { :name => "db server" ,
               :description => "sol's db server",
               :host        => "192.168.10.128", 
               :snmp_read_community  => "sol123", 
               :snmp_write_community => "sol123", 
               :snmp_version => 1, 
               :snmp_timeout => 30,
               :snmp_retry => 2 
               },
    :dev2 => { :name => "rnd server" ,
               :description => "solbrain's rnd server",
               :host        => "192.168.10.11", 
               :snmp_read_community  => "gmdrnr!", 
               :snmp_write_community => "gmdrnr!", 
               :snmp_version => 1, 
               :snmp_timeout => 30,
               :snmp_retry => 2
               }
  },
  :sensors => {
    :s1 => {   :device_id => 1,   
               :svc_plugin_name => "win32_sensor.dll",
               :name => "win32_sensor", 
               :description => "win32 sensor by sol v.1.1",
               :monitoring => 1,
               :msc_interval => 30
           },  
    :s2 => {   :device_id => 1,   
               :svc_plugin_name => "freebsd_sensor.so",
               :name => "freebsd_sensor", 
               :description => "FreeBSD sensor by sol v.1.2",
               :monitoring => 1,
               :msc_interval => 30
           }
  },
  :snmp_get => {
    :sg1 => {  :sensor_id => 1, 
               :name => "cpu user time", 
               :variable  => "ssCpuRawUser", 
               :oid  => "1.3.6.1.2.1.25.3.3.1.2", 
               :oidtext => "iso(1).org(3).dod(6).internet(1).private(4).enterprises(1).ucdavis(2021).systemStats(11).ssCpuRawUser(50)",
               :instance => ".0", 
               :value_type => 1, 
               :comp_method => 1, 
               :unit => "%",
               :scale => 0.0 },
    :sg2 => {  :sensor_id => 1, 
               :name => "cpu user time 2", 
               :variable  => "ssCpuRawUser2", 
               :oid  => "1.3.6.1.4.1.2021.11.51", 
               :oidtext => "iso(1).org(3).dod(6).internet(1).private(4).enterprises(1).ucdavis(2021).systemStats(11).ssCpuRawUser(51)",
               :instance => ".1", 
               :value_type => 1, 
               :comp_method => 1, 
               :unit => "%",
               :scale => 0.0 }
  },
  :snmp_get_value1 => {
    :sgv1 => { :snmpget_id => 1,
               :collect_value => 1.12345,
               :collect_time => Time.now }
  }
}
  
File.open("o2.yaml", "w") {|f| YAML.dump(data, f) }