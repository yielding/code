#!/usr/bin/env ruby

require 'active_record'
require 'yaml'
require 'pp'

dbconf = YAML::load(File.open('db.yaml'))
ActiveRecord::Base.establish_connection(dbconf)

class App < ActiveRecord::Base
  self.primary_key = 'id'
  self.table_name  = 'apps'
end

puts App.count

# pp App.find_each { |s| pp s }
# pp App.find_by(name: "QQ")

# pp App.find([1, 2])
pp App.select("name")
