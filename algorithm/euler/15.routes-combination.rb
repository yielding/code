#!/usr/bin/env ruby19
class Numeric
  def factorial
    self == 0 ? 1 : 1.upto(self).reduce(:*)
  end
end

p 40.factorial/(20.factorial ** 2)
