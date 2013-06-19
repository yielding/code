#!/usr/bin/env ruby2.0

require 'syslog'

log = Syslog.open('adminmate')
i   = 0
loop do
  # log.mask = Syslog::LOG_UPTO(Syslog::LOG_ERR)
  # log.info()
  # log.debug()
  # log.notice()
  # log.warning()
  # log.alert()
  # log.emerg()
  # log.crit()
  log.err("test error #{i}")
  sleep(2)
  i += 1
end
