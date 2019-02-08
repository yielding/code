#!/usr/bin/env ruby

class CoffeeMaker
  attr_accessor :brewer, :warmer

  def trigger
    @brewer.trigger
    @warmer.trigger
  end
end
