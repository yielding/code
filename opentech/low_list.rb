#!/usr/bin/env ruby -wKU

class String
  def has_problem?
    self =~ /\|[:;?<>=]\|/
  end
end

class Problem 
  include Comparable
  attr_accessor :sc, :rc, :signal, :time, :count

  def initialize(line)
    data    = line.split(/\s+|\|/)
    @time   = data[0].slice(1, 8)
    @sc     = data[1]
    @rc     = data[2]
    @signal = data[4]
    @count  = 1
  end

  def afternoon?
    @time.slice(0, 2).to_i > 12
  end

  def <=>(other)     # Comparable
    self.rc <=> other.rc
  end
  
  def eql?(other)    # for set operation
    self.rc.eql?(other.rc) and 
    self.signal.eql?(other.signal)
  end
  
  def hash           # for set operation
    @reader_code.hash
  end
  
  def to_s
    "[#{@time}][#{@sc}][#{@rc}][#{@signal}(#{count})]"
  end
end                                          

class Problems
  attr_accessor :probs
  
  def initialize
    @probs = []
  end
  
  def culmulate(prob)
    element = @probs.find { |p| p.rc == prob.rc and p.signal == prob.signal }
    if element != nil
      element.time   = prob.time
      element.count += 1
    else
      @probs << prob
    end
  end
  
  def print
    @probs.sort.each { |e| puts e.to_s if e.afternoon? }
   end
end


if __FILE__ == $0
  if ARGV.size < 1
    puts "USAGE: low_list.rb filename" 
    exit
  end

  File.open(ARGV[0]) do |f|
    probs = Problems.new
    f.readlines.each { |l| probs.culmulate(Problem.new(l)) } #if l.has_problem? }
    probs.print
  end
end 
