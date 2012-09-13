#!/usr/bin/env ruby19

class Numeric
  def is_palindrome
    s = self.to_s
    l = s.length
    for i in (0...l/2)
      return false if s[i] != s[l-1-i]
    end
    
    true
  end
end

a = [*100..999]
p a.product(a).map    { |e| e.reduce(:*) } \
              .select { |e| e.is_palindrome } \
              .max
