#!/usr/bin/env ruby20

class Numeric
  def is_palindrome
    s = self.to_s
    l = s.length
    s[0..l/2] == s.reverse[0..l/2]
  end
end

a = [*900..999]
p a.product(a).map    { |e| e.reduce(:*) }
              .select { |e| e.is_palindrome }
              .max
