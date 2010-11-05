#!/usr/bin/env ruby

require 'snmp'

host = "192.168.1.21"

manager = SNMP::Manager.new(:Host => host, :Community => 'adminmate', :Port => 161)
response = manager.get(["1.3.6.1.2.1.2.2.10.7",
                        "1.3.6.1.2.1.2.2.11.7",
                        "1.3.6.1.2.1.2.2.12.7",
                        "1.3.6.1.2.1.2.2.13.7",
                        "1.3.6.1.2.1.2.2.14.7",
                        "1.3.6.1.2.1.2.2.16.7",
                        "1.3.6.1.2.1.2.2.17.7",
                        "1.3.6.1.2.1.2.2.18.7",
                        "1.3.6.1.2.1.2.2.19.7",
                        "1.3.6.1.2.1.2.2.20.7",
])
response.each_varbind { |vb| puts vb.to_s }
