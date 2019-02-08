#!/usr/bin/env ruby

class Adapter
  def initialize(device, mapping={})
    @device = device
    @mapping = {}
    mapping.each do |key, value|
      @mapping[key] = normalize(value)
    end
  end

  def method_missing(sym, *args, &block)
    if code = @mapping[sym]
      code.call(@device, *args)
    else
      @device.__send__(sym, *args, &block)
    end
  end

  def normalize(code)
    case code
    when Symbol, String
      lambda { |d, *args| d.__send__(code, *args) }
    when Proc
      code
    else
      fail "Arguments must Symbol, String or Proc"
    end
  end
end
