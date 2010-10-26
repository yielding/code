class Money
  attr_accessor :amount, :currency

  def initialize amount, currency
    @amount   = amount
    @currency = currency
  end

  def == (money)
    @currency == money.currency and @amount == money.amount
  end

  def to_s
    "#{@amount} #{@currency}"
  end
  
  def Money.dollar(amount)
    Money.new(amount, "USD")
  end
  
  def Money.franc(amount)
    Money.new(amount, "CHF")
  end
  
  def Money.won(amount)
    Money.new(amount, "WON")
  end
  
end
