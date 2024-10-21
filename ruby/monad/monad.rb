#!/usr/bin/env ruby

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
    
  # map :: # M a -> (a -> M b) -> M b
  def flat_map(f)
    raise StandardError("flat_map method needs to be implemented")
  end

  # map :: # M a -> (a -> b) -> M b
  def map(f)
    # type of f is a -> b
    # type of f.call(x) is b
    # type of pure(b) is M b
    flat_map(-> (x) { pure(f.call(x)) }) 
  end
end
