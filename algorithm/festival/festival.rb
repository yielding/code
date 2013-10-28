#!/usr/bin/env ruby

class Array
  def sum
    self.reduce(0, :+) 
  end

  def mean
    sum.to_f / size
  end
end

class Run
  def initialize run, line
    @run   = run
    @costs = line.split.map { |n| n.to_i }
  end

  def calc
    @costs.each_cons(@run)
          .map { |a| a.mean }
          .min
  end
end

file   = File.open("in.txt", :encoding => "ASCII")
rec_no = file.readline.chomp.to_i
runs   = []
rec_no.times { |i|
  _, run = file.readline.split.map { |e| e.to_i }
  runs << Run.new(run, file.readline)
}

runs.each { |run| p run.calc }
