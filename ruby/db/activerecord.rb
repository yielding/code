#!/usr/bin/env ruby

require 'active_record'
require 'yaml'
require 'pp'

dbconf = YAML::load(File.open('db.yaml'))
ActiveRecord::Base.establish_connection(dbconf)

class Dept < ActiveRecord::Base
  set_table_name 'dept'
end

puts Dept.count

Dept.find(:all).each { |s| puts s.dname }

#Dept.find(:all, :select => "dname").each { |s| puts s.dname }
