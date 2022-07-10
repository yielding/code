#
# https://www.toptal.com/javascript/option-maybe-either-future-monads-js
#
class Monad
  # pure :: a -> M a
  def self.pure(x)
    raise StandardError("pure method needs to be implemented")
  end
  
  # pure :: a -> M a
  def pure(x)
    self.class.pure(x)
  end
    
  def flat_map(f)
    raise StandardError("flat_map method needs to be implemented")
  end

  # map :: # M a -> (a -> b) -> M b
  def map(f)
    flat_map(-> (x) { pure(f.call(x)) })
  end
end

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
