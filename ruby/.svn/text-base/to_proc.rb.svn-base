#!/usr/bin/env ruby

require 'pp'

# http://ruby.insightbook.co.kr/?p=23
class Symbol
  def to_proc
    Proc.new {|*args| args.shift.__send__(self, *args) }
  end
end

class Person
  attr_accessor :name, :salary
  def initialize name, salary
    @name ||= name
    @salary ||= salary
  end
end

people = [Person.new('leech', 100), Person.new('kamin', 200)]
arr = people.map {|person| person.name }
arr.each {|e| puts e }

arr = people.map(&:name)
arr.each {|e| puts e }
