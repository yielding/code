#!/usr/bin/env ruby1.9

require "sqlite3"

db = SQLite3::Database.new("maptile.db")
db.execute("select zoom, x, y from images") { |row|
  p row
}
