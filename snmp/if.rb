#!/usr/bin/env ruby

require 'snmp'

host = '192.168.1.21'

etherStatistics = "1.3.6.1.2.1.16.1.1.1"
#ifTable_columns = ["ifIndex", "ifDescr", "ifInOctets", "ifOutOctets"]
SNMP::Manager.open(:Host => host) do |manager|
  manager.walk(etherStatistics) do |row|
    row.each { |vb| print "\t#{vb.value}" }
    puts
  end
end
