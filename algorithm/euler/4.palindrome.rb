#!/usr/bin/env ruby19 -wKU

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

#max_palindrome = 0
#a.each { |e| 
  #a.each { |f| 
    #product = e * f
    #if product.is_palindrome
      #max_palindrome = product if product > max_palindrome
    #end
  #}
#}
#p max_palindrome

p a.product(a).map { |e| e.reduce(:*) } \
              .select { |e| e.is_palindrome } \
              .max
