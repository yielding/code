#!/usr/bin/env ruby
# encoding: utf-8

class Object
  def try(method, defaut=nil)
    self.send(method) unless defaut.nil?
  end
end
