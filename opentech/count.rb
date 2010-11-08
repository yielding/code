#!/usr/bin/env ruby
require "enumerator"

filename = ARGV[0]
beg_card = ARGV[1].to_i

class String
  def card?
    self.length > 38
  end
end

def print(cards, count="3")
  lc = 1
  cards.to_a.each_slice(count) do |arr|
    printf("%4d ", lc)
    arr.each { |e| printf "[%d, %4d]", e[0], e[1] }
    puts ""
    lc += 1
  end
end

File.open(filename) do |f|
  cards = Hash.new(0)
  f.readlines.each do |line|
    card_no = line.split(/\s+|\|/)[3].to_i
    cards[card_no] += 1 if line.card? and (card_no >= beg_card) and (card_no < beg_card+50)      
  end
  
  print(cards, 5)
  
  max_count = cards.values.max
  min_count = cards.values.min
  average   = 0
  average   = cards.values.inject {|s, a| s + a} / cards.values.size if cards.values.size != 0
  cut_off   = average * 0.6
  puts "max: #{max_count} min: #{min_count} average:#{average} cutoff=#{cut_off}"

  cards_cutted = cards.select { |k,v| v < cut_off }
  
  print(cards_cutted, 5)
  
end
