#!/usr/bin/env ruby

$: << "."
require 'RubyPerson'

p = RubyPerson::Person.new("leech")

puts p.name()
