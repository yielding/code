#!/usr/bin/env ruby19

require "sqlite3"

db = SQLite3::Database.new("maptile.db")
db.execute("select zoom, x, y from images") { |row|
  p row
}
