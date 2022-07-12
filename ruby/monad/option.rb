require_relative 'monad'

class Option < Monad

  attr_accessor :defined, :value

  # pure :: a -> Option a
  def self.pure(x)
    Some.new(x)
  end

  # pure :: a -> Option a
  def pure(x)
    Some.new(x)
  end
  
  # flat_map :: # Option a -> (a -> Option b) -> Option b
  def flat_map(f)
    if defined
      f.call(value)
    else
      $none
    end
  end
end

class Some < Option
  def initialize(value)
    @value = value
    @defined = true
  end
end

class None < Option
  def initialize()
    @defined = false
  end
end

$none = None.new()
