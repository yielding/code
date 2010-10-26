#!/usr/bin/env ruby

require 'snmp'
require 'timeout'

module Monitoring
  STG_TYPE_OID = "1.3.6.1.2.1.25.2.3.1.2"
  STG_DESC_OID = "1.3.6.1.2.1.25.2.3.1.3"
  STG_UNIT_OID = "1.3.6.1.2.1.25.2.3.1.4"
  STG_SIZE_OID = "1.3.6.1.2.1.25.2.3.1.5"
  STG_USED_OID = "1.3.6.1.2.1.25.2.3.1.6"

  CPU_OID      = "1.3.6.1.2.1.25.3.3.1.2"

  class Windows

    def initialize(host, port=161)
      @host = host
      @port = port
      @manager = SNMP::Manager.new(:Host => @host, :Port => @port, :Community => 'opentechnms')
    end

    def cpu
      snmp_walk(CPU_OID).join(",")
    end

    def memory
      types = snmp_walk(STG_TYPE_OID)
      indices = storage_index_of(types, "vm")
      # ["ss, 90, 10"]
      return storage_get(indices)[0].split[1, 2].join
    end

    def disk
      types = snmp_walk(STG_TYPE_OID)
      indices = storage_index_of(types, "disk")
      return storage_get(indices).join("|")
    end

    private

    def storage_index_of(types, mode)
      pattern = { 'vm' => '3$', 'disk' => '4$' }
      indices = []

      types.each_with_index do |type, i|
        if type.match(pattern[mode])
          indices << i + 1
        end
      end
      indices
    end

    def snmp_walk(mib)
      res = []
      SNMP::Manager.open(:Host => @host, :Community => 'opentechnms') do |manager|
        manager.walk(mib) do |vb|
          res.push(vb.value.to_s)
        end
      end
      res
    end

    def storage_get(indices)
      res = []
      indices.each do |index| 
        response = @manager.get(
          ["#{STG_UNIT_OID}.#{index}",
           "#{STG_SIZE_OID}.#{index}",
           "#{STG_USED_OID}.#{index}",
           "#{STG_DESC_OID}.#{index}"
          ])
        unit = response.varbind_list[0].value.to_f
        size = response.varbind_list[1].value.to_f * unit
        used = response.varbind_list[2].value.to_f * unit
        desc = response.varbind_list[3].value[0, 1]
        free = size-used
        storage = "#{desc}, #{used}, #{free}"
        res << storage
      end
      res
    end
  end
end

if __FILE__ == $0
  #w = Monitoring::Windows.new("220.73.179.70")
  w = Monitoring::Windows.new("192.168.1.169")

  # [w.cpu, w.memory, w.disk].join('#')
  puts w.cpu
  puts w.memory
  puts w.disk

  #begin
  #  timeout(3) do 
  #    puts w.cpu
  #    puts w.memory
  #    puts w.disk
  #  end
  #rescue Timeout::Error

  #rescue

  #ensure

  #end

end
