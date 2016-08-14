
puts "welcome to step3."

def hello3
  puts 'hello3 called.'
end

def tak x, y, z
  if y >= x
    return z
  else
    return tak( tak(x-1, y, z),
                tak(y-1, z, x),
                tak(z-1, x, y))
  end
end

def plus(a,b)
  a + b
end
