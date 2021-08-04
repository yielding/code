#!/usr/bin/env ruby

require 'leveldb'

## make a new database
db = LevelDB::DB.new "/tmp/asdf"

## getting and setting
db.put "it", "works"               # => "works"
db.get "it"                   # => "works"

db["hello"] = "there"              # => "there"
db["hello"]                        # => "there"

db["nonexistent"]                  # => nil

## testing
# puts db.includes? "hello"          # => true
# puts db.contains? "hello"          # => true

## keys and values
puts db.keys                       # => "it", "hello"
puts db.values                     # => "there", "works"
