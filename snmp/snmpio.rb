#!/usr/bin/env ruby

class Snmp
  attr_accessor :host

  def initialize(host, port=161)
    @host      = host
    @version   = "1"
    @community = "sol"
    @value_exp = Regexp.new('(\w+)$')
    @port      = port
    @param     = "-v#{@version} -c#{@community} #{@host}:#{@port}"
  end

  def get(oid)
    @value_exp.match(`snmpget #{@param} #{oid}`)[0].to_f
  end

  def walk(oid)
    result = `snmpwalk #{@param} #{oid}`
    res = result.split(/$/)
    res.collect! { |line| line.lstrip if line.length > 10 }
    res.compact!
    res.collect! { |line| $& if @value_exp =~ line }
  end
end

snmp = Snmp.new("192.168.10.11")
puts snmp.walk("1.3.6.1.4.1.2021.11")
