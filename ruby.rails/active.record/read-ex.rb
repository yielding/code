#!/usr/bin/env ruby

require "active_record"

ActiveRecord::Base.establish_connection(
  :adapter  => "sqlite3",
  :database => "example.db")

class Album < ActiveRecord::Base; has_many :tracks end
class Track < ActiveRecord::Base; belongs_to :album end

Track.all.each { |e| p e.track_number }
