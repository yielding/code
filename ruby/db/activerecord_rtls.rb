#!/usr/bin/env ruby

require 'rubygems'
require 'active_record'
require 'pp'

ActiveRecord::Base.establish_connection(
 :adapter => 'oracle',
 :database => '//oracle.solbrain.co.kr/orcl',
 :username => 'scott',
 :password => 'tiger')

class Rfsignal < ActiveRecord::Base
end

puts Rfsignal.count

Rfsignal.find(:all).each { |s| puts s.rid }

Rfsignal.find(:all, :select => "signal_time").each { |s| puts s.signal_time }
