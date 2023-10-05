#!/usr/bin/env ruby

require 'pathname'

class Numeric
  def is_power_of_two?
    self & (self - 1) == 0
  end

  def to_hex
    "0x#{self.to_s(16)}"
  end
end

yes = 16.is_power_of_two?

puts yes
