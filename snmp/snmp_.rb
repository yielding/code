#!/usr/bin/env ruby

class Snmp
  attr_accessor :host

  def initialize(host="192.168.1.10", port=161)
    @host      = host
    @version   = "1"
    @community = "public"
    @value_exp = Regexp.new('(\d+)$')
    @param     = "-v#{@version} -c#{@community} #{@host}:#{port}"
  end

  def get(oid)
    result = `snmpget #{@param} #{oid}`
    @value_exp.match(result)[0].to_f
  end

  def walk(oid)
    result = `snmpwalk #{@param} #{oid}`
  end

end

snmp = Snmp.new("192.168.1.10")

rawUser_org       = 0.0
rawNice_org       = 0.0
rawSystem_org     = 0.0
rawCpuIdle_org    = 0.0
ioRawSent_org     = 0.0
ioRawReceived_org = 0.0
rawInterrupts_org = 0.0
rawContexts_org   = 0.0
rawSwapIn_org     = 0.0
rawSwapOut_org    = 0.0

interval = 5.0
loop do

  # rawUser_cur       = snmp.get('1.3.6.1.4.1.2021.11.50.0')
  # rawNice_cur       = snmp.get('1.3.6.1.4.1.2021.11.51.0')
  # rawSystem_cur     = snmp.get('1.3.6.1.4.1.2021.11.52.0')
  # rawCpuIdle_cur    = snmp.get('1.3.6.1.4.1.2021.11.53.0')
  # ioRawSent_cur     = snmp.get('1.3.6.1.4.1.2021.11.57.0')
  # ioRawReceived_cur = snmp.get('1.3.6.1.4.1.2021.11.58.0')
  rawInterrupts_cur = snmp.get('1.3.6.1.4.1.2021.11.59.0')
  rawContexts_cur   = snmp.get('1.3.6.1.4.1.2021.11.60.0')
  # rawSwapIn_cur     = snmp.get('1.3.6.1.4.1.2021.11.62.0')
  # rawSwapOut_cur    = snmp.get('1.3.6.1.4.1.2021.11.63.0')

  # printf("rawUser : %5.2f   ", (rawUser_cur - rawUser_org)/ interval)
  # rawUser_org = rawUser_cur;

  # printf("rawNice : %5.2f   ", (rawNice_cur - rawNice_org)/ interval)
  # rawNice_org = rawNice_cur;

  # printf("rawSystem : %5.2f  ", (rawSystem_cur - rawSystem_org)/ interval)
  # rawSystem_org = rawSystem_cur;

  # r = (rawCpuIdle_cur - rawCpuIdle_org) * 100 / (interval * 100.0)
  # printf("rawCpuIdle : %5.2f ", r)
  # rawCpuIdle_org = rawCpuIdle_cur

  # printf("ioRawSent : %5.2f ", (ioRawSent_cur - ioRawSent_org)/ interval)
  # ioRawSent_org = ioRawSent_cur

  # printf("ioRawReceived : %5.2f ", (ioRawReceived_cur - ioRawReceived_org)/ interval)
  # ioRawReceived_org = ioRawReceived_cur

  r = (rawInterrupts_cur - rawInterrupts_org) / interval
  printf("rawInterrupts : %5.2f ", r)
  rawInterrupts_org = rawInterrupts_cur

  printf("rawContexts : %5.2f\n", (rawContexts_cur - rawContexts_org) / interval)
  rawContexts_org = rawContexts_cur

  # printf("rawSwapIn : %5.2f ", (rawSwapIn_cur - rawSwapIn_org)/ interval)
  # rawSwapIn_org = rawSwapIn_cur

  # printf("rawSwapOut : %5.2f \n", (rawSwapOut_cur - rawSwapOut_org)/ interval)
  # rawSwapOut_org = rawSwapOut_cur
  sleep(interval)
end
