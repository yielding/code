#!/usr/bin/env ruby -wKU

require "test/unit"

require "enumerator"

class String 
  def has_problem?
    (self.data? && self.has_prob_mark?) ? true : false
  end
  
  def has_prob_mark?
    self =~ /\|[:;?<>=]\|/
  end
  
  def data?
    self.length > 36
  end
  
  def link_message?
    self.length == 35
  end
  
  def wrong?
    self.length < 35 or self.length > 39
  end
  
  def afternoon?
    afternoon = 12..18
    now = self.slice(1, 2).to_i
    afternoon.include?(now)
  end
  
  def morning?
    morning = 6...11
    now = self.slice(1, 2).to_i
    morning.include?(now)
  end
end

class LogDataLine
  attr_accessor :sc, :rc, :signal, :time, :count

  def initialize(line)
    @time   = line.slice( 1, 8)
    @sc     = line.slice(12, 7)
    @rc     = line.slice(20, 8)
    @signal = line.slice(37, 3)
    @count  = @signal.has_prob_mark? ? 1 : 0
  end
  
  def <=> (line)
    @time <=> line.time
  end
  
  def to_s
    "[#{@time}][#{@sc}][#{@rc}][#{@signal}][#{count}]"
  end
end                                          

class Problems
  def initialize(lines)
    @probs = []
    lines.each { |line| self.culmulate(LogDataLine.new(line)) if line.data? }
  end

  def report
    @probs.sort.each { |e| puts e.to_s }
  end

  def culmulate(prob)
    element = @probs.find { |p| p.rc == prob.rc and p.signal == prob.signal }
    if element != nil
      element.time = prob.time
      element.count += 1
    else
      @probs << prob
    end
  end
end


class TestString < Test::Unit::TestCase
  def setup
    @s1 = "[07:06:47]  EB00048|ZA000040|00000551|3|"
    @s2 = "[11:06:47]  EB00048|ZA000040|00000551|3|"
    @s3 = "[07:06:46]  EB00202|ZC000001|V3.3|\n"
    @s4 = "[11:06:47]  EB00048|ZA000040|00000551|?|"
  end
  
  def test_afternoon_morning
    assert_equal(@s1.afternoon?, false)
    assert_equal(@s1.morning?, true)
  end
  
  def test_wrong
    assert_equal(@s2.afternoon?, false)
    assert_equal(@s2.morning?, false)
  end
  
  def test_data_and_link
    assert_equal(@s1.data?, true)
    assert_equal(@s3.data?, false)
    assert_equal(@s3.link_message?, true)
  end
  
  def test_has_prob
    assert_equal(@s1.has_problem?, false)
    assert_equal(@s4.has_problem?, true)  
  end
  
end

class TestLogLine < Test::Unit::TestCase
  def setup
    @ll1 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|?|")
    @ll2 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|>|")
    @ll3 = LogDataLine.new("[11:06:47]  EB00048|ZA000040|00000551|2|")
  end
  
  def test_has_prob
    assert_equal(@ll1.count, 1)
    assert_equal(@ll2.count, 1)
    assert_equal(@ll3.count, 0)
  end
  
end

class TestProblems < Test::Unit::TestCase
  def setup
    arr = ["[11:06:47]  EB00048|ZA000040|00000551|?|",
           "[11:06:47]  EB00048|ZA000040|00000551|>|",
           "[11:06:47]  EB00048|ZA000040|00000551|2|"
          ]
          
    @ps = Problems.new(arr)
  end
  
  def test_ok
  end
  
end

if __FILE__ == $0
  if ARGV.size < 1
    puts "USAGE: log_analysis.rb op filename opt" 
    exit
  end

  File.open(ARGV[0]) do |f|
    probs = Problems.new(f.readlines)
    probs.report
  end
end