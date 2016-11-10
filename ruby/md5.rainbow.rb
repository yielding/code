#!/usr/bin/env ruby

require 'digest'
require 'sqlite3'

db = SQLite3::Database.new("/Users/yielding/Desktop/static.db")

md5 = Digest::MD5.new
0.upto(9999) { |no|
  no_s = sprintf("%04d", no)
  md5.update(no_s)
  db.execute("insert into [reverse_md5] (hash, passcode) values(?, ?)", [md5.hexdigest, no_s])
  p no_s
}
