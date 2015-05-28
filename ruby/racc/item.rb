#!/usr/bin/env ruby

require 'forwardable'
require 'pp'
    
class Item
  attr_reader :name

  def initialize(name)
    @name = name
  end
end

class Catalog
  extend Forwardable

  def initialize
    @items = []
  end

  def_delegators :@items, :size, :<<, :[]
end

c = Catalog.new

c << Item.new("leech")
c << Item.new("kamin")

pp c
