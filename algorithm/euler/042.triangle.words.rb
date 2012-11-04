#!/usr/bin/env ruby19

class String
  def word_value
    self.each_byte.reduce(0) { |s, b| s + b - 64 }
  end
end

tri_nums = (1..100).map { |e| e * (e + 1) / 2 }
words = File.read("./words.txt").gsub(/"/, '').split(",")

p words.select { |w| tri_nums.include?(w.word_value) }.size
