#!/usr/bin/env ruby
require 'rubygems'
require 'active_record'
require 'yaml'
require 'logger'
require 'pp'

ActiveRecord::Base.establish_connection(
 :adapter => 'mysql',
 :database => 'ss16',
 :username => 'root',
 :password => 'sol123',
 :host => 'rnd.solbrain.co.kr')

class Tag < ActiveRecord::Base
end

class Rfsignal < ActiveRecord::Base
end

#Rfsignal.find(:all).each do |s|
#  pp s
#end

#Tag.find(:all).each do |s|
#  pp s
#end

Tag.find(:all).each do |s|
  v = s.cps.split(",")[0].to_f * 1.1 / 1024
  #Rfsignal.find(:all, :conditions => "tid=#{s.tid} and value>#{v}")
  count = Rfsignal.count(:conditions => "tid=#{s.tid} and value>#{v}")
  puts "#{s.tid}, #{count}" 
end
