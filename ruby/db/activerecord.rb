#!/usr/bin/env ruby

require 'active_record'
require 'yaml'
require 'pp'

dbconf = YAML::load(File.open('db.yaml'))
ActiveRecord::Base.establish_connection(dbconf)

class User < ActiveRecord::Base
  self.table_name = 'user'
end

puts User.count

#Dept.find(:all).each { |s| puts s.dname }

#Dept.find(:all, :select => "dname").each { |s| puts s.dname }
