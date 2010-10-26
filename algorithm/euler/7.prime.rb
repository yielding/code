class Fixnum
  # no 1
  def prime?
    return false if self < 2
    2.upto(self/2) { |n| return false if self % n == 0 }
    true
  end
  
  # no 2
  def prime2?
    return false if self < 2
    return true  if self < 4
    return false if self % 2 == 0
    return true  if self < 9
    return false if self % 3 == 0
    
    r = Math.sqrt(self).floor
    f = 5
    while f <= r
      return false if self % f == 0
      return false if self % (f + 2) == 0
      f += 6
    end
    true
  end
end

def nth_prime? n
  i = 0
  current = 1
  while i<n
    if current.prime?
      i += 1
      puts "#{i} th => #{current}" if i % 100 == 0
    end
    current += 1
  end
  current-1
end

puts  nth_prime? 100001
