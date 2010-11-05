require 'string_helper'

module Opentech

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
      p = @probs.find { |p| p.rc == prob.rc and p.signal == prob.signal }
      if p != nil
        p.time = prob.time
        p.count += 1
      else
        @probs << prob
      end
    end
  end

end
