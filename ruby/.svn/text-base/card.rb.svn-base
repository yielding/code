#!/usr/bin/env ruby

Card = Struct.new(:rank, :suit)

dealer = Fiber.new do
  ranks = (1..13).to_a
  suits = [:club, :diamond, :heart, :spade]
  pack  = ranks.product(suits).map {|card| Card.new(*card)}
  all_cards = pack * 7
  cards = []
  loop do
    cards = all_cards.shuffle if cards.size < 2*52
    Fiber.yield cards.pop
  end
end  
