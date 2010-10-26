#!/usr/bin/env ruby -wKU

require 'snmp'

class Snmp
  def initialize(host='yielding.xp')
    @manager = SNMP::Manager.new(:Host => host, :Port => 161)
  end                                     

  protected
  def walk(oid)
  end          

  def get(oid)
  end         

end

class WinStorage < Snmp
  STG_TYPE_OID = "1.3.6.1.2.1.25.2.3.1.2"    
  STG_DESC_OID = "1.3.6.1.2.1.25.2.3.1.3"
  STG_UNIT_OID = "1.3.6.1.2.1.25.2.3.1.4"
  STG_SIZE_OID = "1.3.6.1.2.1.25.2.3.1.5"
  STG_USED_OID = "1.3.6.1.2.1.25.2.3.1.6"

  def initialize()
  end

  private
  def index_of(types)
    indices = []

    types.each_with_index { |type, i| 
      indices << i + 1 if type.match('4$')
    }
    indices
  end

end