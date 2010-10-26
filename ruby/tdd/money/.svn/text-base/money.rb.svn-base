require 'profiler'

class Expression
  def reduce(bank, to); end
  def + (addend); end
  def * (multiplier); end
end

class Pair
  def initialize(from, to)
    @from, @to = from, to
  end

  def hash
    @from.hash + @to.hash
  end

  def eql?(rhs)
    hash == rhs.hash
  end

  def == (rhs)
    @from == rhs.from && @to == rhs.to
  end
end

class Money < Expression
  attr_reader :amount, :currency

  def initialize(amount, currency)
    @amount = amount
    @currency = currency
  end

  def == (rhs)
    (@amount == rhs.amount) && (@currency == rhs.currency)
  end

  def * (multiplier)
    Money.new(@amount * multiplier, @currency);
  end

  def + (addend)
    Sum.new(self, addend)
  end

  def reduce(bank, to)
    rate = bank.rate(@currency, to)
    Money.new(@amount / rate, to)
  end

  def Money.dollar(amount)
    Money.new(amount, "USD")
  end

  def Money.franc(amount)
    Money.new(amount, "CHF")
  end
end

class Sum < Expression
  attr_reader :addend, :augend

  def initialize(augend, addend)
    @augend, @addend = augend, addend
  end

  def reduce(bank, to)
    amount = @augend.reduce(bank, to).amount + 
             @addend.reduce(bank, to).amount
    Money.new(amount, to)
  end

  def + (addend)
    Sum.new(self, addend)
  end

  def * (multiplier)
    Sum.new(@augend * multiplier, @addend * multiplier)
  end

end

class Bank
  def initialize
    @rates = Hash.new("currency rate")
  end

  def reduce(source, to)
    # if source.class == Money
    #   return source
    # end
    source.reduce(self, to)
  end

  def add_rate(from, to, rate)
    @rates[Pair.new(from, to)] = rate
  end

  def rate(from, to)
    return 1 if from == to

    rate = @rates[Pair.new(from, to)]
    rate.to_i
  end

end