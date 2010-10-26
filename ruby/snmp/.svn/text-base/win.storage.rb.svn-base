require 'snmp'

$host = ARGV[0] || 'yielding.xp'

stg_type_oid = "1.3.6.1.2.1.25.2.3.1.2"    
stg_desc_oid = "1.3.6.1.2.1.25.2.3.1.3"
stg_unit_oid = "1.3.6.1.2.1.25.2.3.1.4"
stg_size_oid = "1.3.6.1.2.1.25.2.3.1.5"
stg_used_oid = "1.3.6.1.2.1.25.2.3.1.6"

def snmp_walk(mib)
  res = []
  SNMP::Manager.open(:Host => $host) do |manager|
    manager.walk(mib) do |vb|
      res.push(vb.value.to_s)
    end
  end  
  res
end
              
def disk_index_of(types)
  indices = []
  
  types.each_with_index do |type, i|  
    if type.match('4$')
      indices << i + 1
    end
  end
  indices 
end
 
types   = snmp_walk(stg_type_oid)                          
indices = disk_index_of(types)                            
unless indices.empty?
  manager = SNMP::Manager.new(:Host => $host, :Port => 161)
  indices.each do |index| 
    response = manager.get(
     [ "#{stg_unit_oid}.#{index}",
       "#{stg_size_oid}.#{index}",
       "#{stg_used_oid}.#{index}",
       "#{stg_desc_oid}.#{index}"
      ])
    unit = response.varbind_list[0].value.to_f
    size = response.varbind_list[1].value.to_f * unit
    used = response.varbind_list[2].value.to_f * unit
    desc = response.varbind_list[3].value[0, 1]
    free = size-used
    perc = free / size * 100
    puts "#{desc}: total=#{size}, used=#{used}, free=#{free}, free % =#{perc}"
  end
end
