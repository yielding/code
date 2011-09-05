#!/usr/bin/env ruby -wKU

class Dispatcher1
  def initialize(origin, &block)
    @origin = origin
    @block  = block if block_given?
  end

  def dispatch
    self.instance_eval &@block
  end
end

p Dispatcher1.new(40) { @origin + 2 }.dispatch

#
# far better faster with no object(T_NODE) allocation
#
class Dispatcher2
  def initialize(origin, &block)
    @origin = origin
    implementation(block) if block_given?
  end

  private
  def implementation(block)
    mod = Module.new
    mod.send(:define_method, :dispatch, block)
    self.extend mod
  end
end

p Dispatcher2.new(40) { @origin + 2 }.dispatch
