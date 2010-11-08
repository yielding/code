#!/usr/bin/env ruby

require "mkmf"

c_program = ARGV.first
`swig -ruby -c++ #{c_program}.i`
have_library('boost_thread-mt')
have_library('boost_system-mt')
create_makefile(c_program)
`make`
#puts `spec -f s -c #{c_program}_spec.rb`
#puts `ruby #{c_program}_test.rb`
