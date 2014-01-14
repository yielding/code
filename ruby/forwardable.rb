#!/usr/bin/env ruby
# encoding: utf-8

require "forwardable"

class Stack 
  extend Forwardable
  
  def_delegators :@data, :push, :pop, :size, :first, :empty?
  
  def initialize
    @data = []
  end
end

stack = Stack.new
stack.push 1
stack.push 2

p stack
