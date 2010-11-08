#!/usr/bin/env ruby

require "enumerator"
require "string_helper"

module Opentech

  class CountRange
    TEST_COUNT = 50
    attr_accessor :max, :min, :average

    def initialize lines, beg_card
      @cards = Hash.new(0)
      lines.each do |line|
        card_no  = line.split(/\s+|\|/)[3].to_i
        in_range = line.data? and (card_no >= beg_card) and (card_no < beg_card+TEST_COUNT)
        @cards[card_no] += 1 if in_range
      end

      @max = @cards.values.max
      @min = @cards.values.min
      size = @cards.values.size
      @average = 0.0
      @average = @cards.values.inject { |s, a| s + a } / size if size != 0
      @cutoff  = @average * 0.6
      @cards_cutted = @cards.select { |k, v| v < @cutoff }
    end

    def report
      self.print(@cards)
      TODO
      print "#{}{}"
      self.print(@cards_cutted)
    end

    protected
    def print(cards, count=3)
      lc = 1
      cards.to_a.each_slice(count) do |arr|
        printf("%4d ", lc)
        arr.each { |e| printf "[%d, %4d]", e[0], e[1] }
        puts ""
        lc += 1
      end
    end
  end
  
end
