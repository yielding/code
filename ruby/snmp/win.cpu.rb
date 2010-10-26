#!/usr/bin/env ruby
#
require 'snmp'

$host   = ARGV[0] || '192.168.10.127'
cpu_oid = "1.3.6.1.2.1.25.3.3.1.2"

def snmp_walk(mib)
  res = []
  SNMP::Manager.open(:Host => $host) do |manager|
    manager.walk(mib) do |vb|
      res.push(vb.value.to_s)
    end
  end  
  res
end


puts snmp_walk(cpu_oid).join(',')
