#!/usr/bin/env ruby

require 'activeresource'

class Event < ActiveResource::Base
  self.site = "http://localhost:3000"
end

events = Event.find(:all)
puts events.map(&:name)

e = Event.find(1)
e.budget = 12345
e.save

