require_relative "money"

class Account
  attr_accessor :balance

  def initialize 
    @balance = Money.new(0, :dollar)
  end
end
