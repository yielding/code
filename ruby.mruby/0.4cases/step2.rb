
puts "welcome to step2."

def fib(n)
  if n<2
    n
  else
    fib(n-2)+fib(n-1)
  end
end

(0..10).each do |n|
  puts "fib(#{n}) = " + fib(n).to_s
end

##########################################################################
# http://blog.headius.com/2009/04/how-jruby-makes-ruby-fast.html

# Takeuchi function performance, tak(24, 16, 8)
def tak x, y, z
  if y >= x
    return z
  else
    return tak( tak(x-1, y, z),
                tak(y-1, z, x),
                tak(z-1, x, y))
  end
end

#require "benchmark"

N = 1 #(ARGV.shift || 1).to_i
#Benchmark.bm do |make|
  N.times do |n|
    puts "n=#{n}"
    #make.report do
      i = 0
      while i<10
        tak(24, 16, 8)
        i+=1
        puts "i=#{i}"
      end
    #end
  end
#end

