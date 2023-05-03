#!/usr/bin/env ruby

require "active_record"

ActiveRecord::Base.logger = Logger.new(File.open('db.log', 'w'))
ActiveRecord::Base.establish_connection(
  :adapter  => "sqlite3",
  :database => "example.db")

ActiveRecord::Schema.define do
  unless ActiveRecord::Base.connection.tables.include? 'albums'
    create_table :albums do |table|
      table.column :title,     :string
      table.column :performer, :string
    end
  end

  unless ActiveRecord::Base.connection.tables.include? 'tracks'
    create_table :tracks do |table|
      table.column :album_id,     :integer
      table.column :track_number, :integer
      table.column :title,        :string
    end
  end
end

class Album < ActiveRecord::Base
  has_many :tracks
end

class Track < ActiveRecord::Base
  belongs_to :album
end

unless Album.find_by_title('In Utero')
  album = Album.create(
    :title     => 'In Utero',
    :performer => 'Nirvana'
  )

  track_listing = [
    nil,
    'Serve the Servants',
    'Scentless Apprentice',
    'Heart-Shaped Box',
    'Rape Me',
    'Frances Farmer',
    'Dumb',
    'Very Ape',
    'Milk It',
    'Pennyroyal Tea',
    'Radio Friendly Unit Shifter',
    'Tourettes',
    'All Apologies'
  ]

  track_listing.each_with_index do |value, index|
    album.tracks.create(:track_number => index, :title => value) unless index === 0
  end
end

unless Album.find_by_title('La-te-ra-lus')
  album = Album.create(
    :title     => 'La-te-ra-lus',
    :performer => 'Tool'
  )

  track_listing = [
    nil,
    'The Grudge',
    'Eon Blue Apocalypse',
    'The Patient',
    'Mantra',
    'Schism',
    'Parabol',
    'Parabola',
    'Ticks & Leeches',
    'Lateralus',
    'Disposition',
    'Reflection',
    'Triad',
    'Faaip de Oiad'
  ]

  track_listing.each_with_index do |value, index|
    album.tracks.create(:track_number => index, :title => value) unless index === 0
  end
end

begin
  p Album.find(1).tracks.length # 13 (on first run of this script it's fine, but next run we've deleted the record)
rescue ActiveRecord::RecordNotFound
  p 'We just rescued a "RecordNotFound" error'
end

p Album.find(2).tracks.length # 14
p Album.find_by_title('La-te-ra-lus').title # "La-te-ra-lus"
p Track.find_by_title('Very Ape').album_id # 1
p Album.all # ActiveRecord::Relation => complete set of database records
p Album.all.length # 2
p Track.all # ActiveRecord::Relation => complete set of database records
p Track.where(title: 'Triad') # ActiveRecord::Relation => single record
p Track.where(track_number: 6..8) # returns tracks 6 to 8 from all albums
p Album.first # returns first record (calling `Album.find(:first|:last) is deprecated`)
p Album.last # returns last record

if Album.all.length > 1
  p Album.first.delete # delete the first record
end

p Album.all # now we'll see there is only one record remaining

track_to_be_modified = Track.where(title: 'The Grudge').first
track_to_be_modified.title = 'Grudgeola'
track_to_be_modified.save
p track_to_be_modified # displays modified record
p Track.where(title: 'The Grudge') # empty Array (not found as we've overridden the original record)
p Track.where(title: 'Grudgeola') # displays Array of records found (only one, the modified record)
