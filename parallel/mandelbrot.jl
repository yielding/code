
using Images

@everywhere begin

function grayvalue(n)
    round(UInt8, clamp(n, 0, typemax(UInt8)))
end
    
function mandelbrotorbit(f, seed, bound, bailout=100, itmap=(n,zn,b)->n)
    z = f(seed)
    #z = seed^2 + c
    for k = 1:bailout
        if abs(z) > bound
            return itmap(k, z, bailout)
        end
        #z = z^2 + c
        z = f(z)
    end
    
    return -Inf
end

import Base.linspace

function linspace(start::Complex, finish::Complex, n::Integer, m::Integer)
    realParts = linspace(real(start), real(finish), n)
    complexParts = [Complex(0, b) for b=linspace(imag(start), imag(finish), m)]
    [ a + b for a=realParts, b=complexParts ]
end

normalized_iterations(n, zn, bailout) = n + (log(log(bailout))-log(log(abs(zn))))/(log(2))

end

# prepare input data
points = convert(SharedArray, linspace(-2.5-1.25im, 1.0+1.25im, 3500, 2500))

# prepare results array
colors = SharedArray(UInt8, size(points))

function mandelbrot(points::SharedArray{Complex128,2}, colors::SharedArray{UInt8,2})
    const BAILOUT = 200

    f(c) = mandelbrotorbit(z -> z^2 + c, 0.0im, 2.0, BAILOUT, normalized_iterations)
    #f(c) = mandelbrotorbit(c, 0.0im, 2.0, BAILOUT, normalized_iterations)
  
    @sync @parallel for j=1:size(points,2)
        for k=1:size(points,1)
            @inbounds colors[k,j] = grayvalue(f(points[k,j]))
        end
    end
end

@time mandelbrot(points, colors) 

image = grayim(sdata(colors))
imwrite(image, "mandelbrot_gray.png");
