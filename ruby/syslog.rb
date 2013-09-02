#!/usr/bin/env ruby

require 'syslog'

log = Syslog.open("test")   # "test" is the app name
# log.debug("Warm and fuzzy greetings from your program")
# log.info("Program starting")
# log.notice("I said 'Hello!'")
# log.warning("If you don't respond soon, I'm quitting")
# log.err("You haven't responded after %d milliseconds", 7)
# log.alert("I'm telling your mother...")
# log.emerg("I'm feeling totally crushed")
log.crit("yielding....")
