#!/usr/bin/env macruby

framework "Cocoa"

class Dog
  attr_accessor :number
  def initialize 
    @number = 0
  end
end

dog = Dog.alloc.init
dog.number = 1

puts dog.number
