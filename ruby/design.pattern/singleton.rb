#!/usr/bin/env ruby

=begin rdoc
  
# Rubymotion way
class Foo
  def self.instance
    Dispatch.once { @instance ||= new }
    @instance
  end

  def initialize
    @refs = [] 
  end

  def assign(obj)
    @refs << obj
  end

  def clear()
    @refs.clear
  end
end

foo = Foo.instance
p foo

=end


require "singleton"

class Foo 
  include Singleton

  def initialize
    @refs = [] 
  end

  def assign(obj)
    @refs << obj
  end

  def clear()
    @refs.clear
  end
end

foo = Foo.instance
p foo
