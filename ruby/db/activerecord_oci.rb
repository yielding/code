#!/usr/bin/env ruby

require 'active_record'
require 'pp'

ActiveRecord::Base.establish_connection(
 :adapter  => 'oracle',
 :database => "//oracle.solbrain.co.kr/orcl",
 :username => 'scott',
 :password => 'tiger')

class Dept < ActiveRecord::Base
  set_table_name 'dept'
end

puts Dept.count

Dept.find(:all).each { |s| puts s.dname }

Dept.find(:all, :select => "dname").each { |s| puts s.dname }
