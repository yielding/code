#!/usr/bin/env ruby

require 'snmp'

include SNMP

host = "192.168.10.128"
p host

Manager.open(:Host => host, :Community => "sol123", :Port => 161) do |manager|
  manager.walk("1.3.6.1.4.1") { |vb| puts vb }
end

# Manager.open(:Host => host) do |manager|
#   manager.walk(["ifIndex", "ifDescr"]) do |ifIndex, ifDescr| 
#     puts "#{ifIndex} #{ifDescr}"
#   end
# end
