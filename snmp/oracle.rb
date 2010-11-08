#!/usr/bin/env ruby

$instance  = ".2";
$host      = "192.168.1.6"

oraDbSysConsistentChanges      =  "1.3.6.1.4.1.111.4.1.1.1.1"  + $instance
oraDbSysConsistentGets         =  "1.3.6.1.4.1.111.4.1.1.1.2"  + $instance
oraDbSysDbBlockChanges         =  "1.3.6.1.4.1.111.4.1.1.1.3"  + $instance
oraDbSysDbBlockGets            =  "1.3.6.1.4.1.111.4.1.1.1.4"  + $instance
oraDbSysFreeBufferInspected    =  "1.3.6.1.4.1.111.4.1.1.1.5"  + $instance
oraDbSysFreeBufferRequested    =  "1.3.6.1.4.1.111.4.1.1.1.6"  + $instance
oraDbSysParseCount             =  "1.3.6.1.4.1.111.4.1.1.1.7"  + $instance
oraDbSysPhysReads              =  "1.3.6.1.4.1.111.4.1.1.1.8"  + $instance
oraDbSysPhysWrites             =  "1.3.6.1.4.1.111.4.1.1.1.9"  + $instance
oraDbSysRedoEntries            =  "1.3.6.1.4.1.111.4.1.1.1.10" + $instance
oraDbSysRedoLogSpaceRequests   =  "1.3.6.1.4.1.111.4.1.1.1.11" + $instance
oraDbSysRedoSyncWrites         =  "1.3.6.1.4.1.111.4.1.1.1.12" + $instance
oraDbSysSortsDisk              =  "1.3.6.1.4.1.111.4.1.1.1.13" + $instance
oraDbSysSortsMemory            =  "1.3.6.1.4.1.111.4.1.1.1.14" + $instance
oraDbSysSortsRows              =  "1.3.6.1.4.1.111.4.1.1.1.15" + $instance
oraDbSysTableFetchRowid        =  "1.3.6.1.4.1.111.4.1.1.1.16" + $instance
oraDbSysTableFetchContinuedRow =  "1.3.6.1.4.1.111.4.1.1.1.17" + $instance
oraDbSysTableScanBlocks        =  "1.3.6.1.4.1.111.4.1.1.1.18" + $instance
oraDbSysTableScanRows          =  "1.3.6.1.4.1.111.4.1.1.1.19" + $instance
oraDbSysTableScansLong         =  "1.3.6.1.4.1.111.4.1.1.1.20" + $instance
oraDbSysTableScansShort        =  "1.3.6.1.4.1.111.4.1.1.1.21" + $instance
oraDbSysUserCalls              =  "1.3.6.1.4.1.111.4.1.1.1.22" + $instance
oraDbSysUserCommits            =  "1.3.6.1.4.1.111.4.1.1.1.23" + $instance
oraDbSysUserRollbacks          =  "1.3.6.1.4.1.111.4.1.1.1.24" + $instance
oraDbSysWriteRequests          =  "1.3.6.1.4.1.111.4.1.1.1.25" + $instance

class Snmp
  attr_accessor :host

  def initialize(host="192.168.1.6")
    @host      = host
    @version   = "1"
    @community = "public"
    @value_exp = Regexp.new('(\d+)$')
  end

  def get(oid)
    result = `snmpget -v#{@version} -c#{@community} #{@host} #{oid}`
    @value_exp.match(result)[0].to_f
  end

end

interval = 5.0

snmp = Snmp.new($host)
loop do 
  sleep(interval)
  consistentChanges      = snmp.get(oraDbSysConsistentChanges)
  userCalls              = snmp.get(oraDbSysUserCalls)
  dbBlockGets            = snmp.get(oraDbSysDbBlockGets)
  userCommits            = snmp.get(oraDbSysUserCommits)
  consistentGets         = snmp.get(oraDbSysConsistentGets)
  dbBlockGets            = snmp.get(oraDbSysDbBlockGets)
  physReads              = snmp.get(oraDbSysPhysReads)
  userCalls              = snmp.get(oraDbSysUserCalls)
  userCommits            = snmp.get(oraDbSysUserCommits)
  dbBlockChanges         = snmp.get(oraDbSysDbBlockChanges)
  tableFetchContinuedRow = snmp.get(oraDbSysTableFetchContinuedRow)
  tableFetchRowid        = snmp.get(oraDbSysTableFetchRowid)
  tableScanRows          = snmp.get(oraDbSysTableScanRows)
  redoLogSpaceRequests   = snmp.get(oraDbSysRedoLogSpaceRequests)
  redoEntries            = snmp.get(oraDbSysRedoEntries)
  sortsDisk              = snmp.get(oraDbSysSortsDisk)
  sortsMemory            = snmp.get(oraDbSysSortsMemory)
  userCommits            = snmp.get(oraDbSysUserCommits)
  userCalls              = snmp.get(oraDbSysUserCalls)
  parseCount             = snmp.get(oraDbSysParseCount)
  userRollbacks          = snmp.get(oraDbSysUserRollbacks)

  puts "\n-----------------------------------------------------\n"
  printf("%s = %f\n", "consistentChanges     ", consistentChanges)
  printf("%s = %f\n", "dbBlockGets           ", dbBlockGets)
  printf("%s = %f\n", "consistentGets        ", consistentGets)
  printf("%s = %f\n", "dbBlockChanges        ", dbBlockChanges)
  printf("%s = %f\n", "dbBlockGets           ", dbBlockGets)
  printf("%s = %f\n", "parseCount            ", parseCount)
  printf("%s = %f\n", "physReads             ", physReads)
  printf("%s = %f\n", "redoEntries           ", redoEntries)
  printf("%s = %f\n", "redoLogSpaceRequests  ", redoLogSpaceRequests)
  printf("%s = %f\n", "sortsDisk             ", sortsDisk)
  printf("%s = %f\n", "sortsMemory           ", sortsMemory)
  printf("%s = %f\n", "tableFetchContinuedRow", tableFetchContinuedRow)
  printf("%s = %f\n", "tableFetchRowid       ", tableFetchRowid)
  printf("%s = %f\n", "tableScanRows         ", tableScanRows)
  printf("%s = %f\n", "userCalls             ", userCalls)
  printf("%s = %f\n", "userCommits           ", userCommits)
  printf("%s = %f\n", "userRollbacks         ", userRollbacks)
  
  block_changes_per_transaction = consistentChanges / userCalls
  block_get_rate                = (consistentChanges + dbBlockGets) / interval
  block_visits_per_transaction  = (dbBlockGets + consistentChanges) / userCommits
  cache_hit_ratio               = 100 * ((consistentGets + dbBlockGets - physReads) / (consistentGets + dbBlockGets))
  calls_per_transaction         = userCalls / userCommits
  changed_block_ratio           = 100 * (dbBlockChanges / (dbBlockGets + consistentGets))
  consistent_change_ratio       = 100 * (consistentChanges / consistentGets)
  continued_row_ratio           = 100 * (tableFetchContinuedRow / (tableFetchRowid + tableScanRows))
  redo_log_space_wait_ratio     = 100 * (redoLogSpaceRequests / redoEntries)
  row_source_ratio              = 100 * (tableScanRows / (tableFetchRowid + tableScanRows))
  sort_overflow_ratio           = 100 * (sortsDisk / (sortsMemory + sortsDisk))
  transaction_rate              = userCommits
  user_call_rate                = userCalls
  user_calls_per_parse          = userCalls / parseCount
  rollback_ratio                = 100 * (userRollbacks / (userCommits + userRollbacks))
  
  puts "\n-----------------------------------------------------\n"
  printf("%s = %f\n", "block_changes_per_transaction", block_changes_per_transaction)
  # printf("%s = %f\n", "block_get_rate               ", block_get_rate)
  printf("%s = %f\n", "block_visits_per_transaction ", block_visits_per_transaction)
  printf("%s = %f\n", "cache_hit_ratio              ", cache_hit_ratio)
  printf("%s = %f\n", "calls_per_transaction        ", calls_per_transaction)
  printf("%s = %f\n", "changed_block_ratio          ", changed_block_ratio)
  printf("%s = %f\n", "consistent_change_ratio      ", consistent_change_ratio)
  printf("%s = %f\n", "continued_row_ratio          ", continued_row_ratio)
  printf("%s = %f\n", "redo_log_space_wait_ratio    ", redo_log_space_wait_ratio)
  printf("%s = %f\n", "row_source_ratio             ", row_source_ratio)
  printf("%s = %f\n", "sort_overflow_ratio          ", sort_overflow_ratio)
  # printf("%s = %f\n", "transaction_rate             ", transaction_rate)
  # printf("%s = %f\n", "user_call_rate               ", user_call_rate)
  printf("%s = %f\n", "user_calls_per_parse         ", user_calls_per_parse)
  printf("%s = %f\n", "rollback_ratio               ", rollback_ratio)
end
