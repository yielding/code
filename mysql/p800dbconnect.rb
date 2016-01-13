#!/usr/bin/env ruby

require "active_record"
require "pp"

ActiveRecord::Base.establish_connection(
  :adapter  => "mysql",
  :host     => "localhost",
  :username => "root",
  :password => "Kamin1974",
  :database => "students"
)

class Rubyist < ActiveRecord::Base
end

#Rubyist.create(:name => 'Luc Juggery', :city => "Nashville, Tenessee")
#Rubyist.create(:name => 'Sunil Kelkar', :city => "Pune, India")
#Rubyist.create(:name => 'Adam Smith', :city => "San Fransisco, USA")

Rubyist.all.each do |p|
  puts %{#{p.name} stays in #{p.city}}
end
