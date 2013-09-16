#!/usr/bin/env ruby

require 'barracuda'
require 'benchmark'

include Barracuda

prog = Program.new <<-eof
  __kernel sum(__global float *out, __global int *in, int total) {
    int i = get_global_id(0);
    if (i < total) out[i] = ((float)in[i] + 0.5) / 3.8 + 2.0;
  }
eof

arr = (1..3333333).to_a
input  = Buffer.new(arr)
output = OutputBuffer.new(:float, arr.size)
 
Benchmark.bmbm do |x|
  x.report("cpu") { arr.map {|x| (x.to_f + 0.5) / 3.8 + 2.0 } }
  x.report("gpu") { prog.sum(output, input, arr.size) }
end
