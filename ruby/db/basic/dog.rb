#!/usr/bin/env ruby

require "active_record"
require "pp"

conn = { adapter: 'sqlite3', database: 'db/dogs.db' }
ActiveRecord::Base.establish_connection(conn)

ActiveRecord::Schema.define do
  create_table :dogs do |t|
    t.string   :name
    t.date     :dob
  end
end

class Dog < ActiveRecord::Base
end

dog = Dog.create({ name: 'Scruffy', dob:  '2012-01-01' })

Dog.find_each { |dog| pp dog }
