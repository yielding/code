#!/usr/bin/env ruby

class Object
  def try(method, defaut=nil)
    self.send(method) unless defaut.nil?
  end
end
