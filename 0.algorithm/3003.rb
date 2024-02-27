#!/usr/bin/env ruby

def solve chess, input
  a = chess.split.map { |e| e.to_i }
  b = input.split.map { |e| e.to_i }
  a.zip(b)
   .map    { |e1, e2| e1 - e2 }
   .reduce { |acc, e| "#{acc} #{e.to_s}" }
   .strip
end

chess = "1 1 2 2    2 8"
input = "0 1 2 2 2 7"

puts solve(chess, input)