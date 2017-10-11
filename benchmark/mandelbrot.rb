#!/usr/bin/env ruby

BAILOUT = 16
MAX_ITERATIONS = 1000

class Iterator
	def initialize
		puts "Rendering"
		for y in -39..39 do
      putc ?\n
			for x in -39..39 do
				i = mandelbrot(x/40.0,y/40.0)
        if (i == 0)
          putc '*'
        else
          putc ' '
        end
			end
		end
	end

	def mandelbrot(x,y)
		cr = y-0.5
		ci = x
		zi = 0.0
		zr = 0.0
		i = 0
		
		while true
			i += 1
			temp = zr * zi
			zr2 = zr * zr
			zi2 = zi * zi
			zr = zr2 - zi2 + cr
			zi = temp + temp + ci
			return i if (zi2 + zr2 > BAILOUT)
			return 0 if (i > MAX_ITERATIONS)
		end
	
	end

end

time = Time.now
Iterator.new
puts
puts "Ruby Elapsed %f" % (Time.now - time)
