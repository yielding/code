#!/usr/bin/env ruby

require_relative './monad'
require_relative './either'
require_relative './option'

class Future < Monad
  attr_accessor :subscribers, :cache, :semaphore

  # initialize :: ((Either err a -> void) -> void) -> Future (Either err a)
  def initialize(f)
    @subscribers = []
    @cache = $none
    @semaphore = Queue.new # m-producer, n-consumer queue
    @semaphore.push(nil)
    f.call(method(:callback))
  end

  def self.async(f, *args)
    Future.new(-> (cb) {
      Thread.new {
        begin
          cb.call(Right.new(f.call(*args)))
        rescue => e
          cb.call(Left.new(e))
        end  
      }
    })
  end

  # pure :: a -> Future a
  def self.pure(value)
    Future.new(-> (cb) { cb.call(Either.pure(value)) })
  end

  # pure :: a -> Future a
  def pure(value)
    self.class.pure(value)
  end

  # map :: # M a -> (a -> b) -> M b
  # def map(f)
  #   # type of f is a -> b
  #   # type of f.call(x) is b
  #   # type of pure(b) is M b
  #   flat_map(-> (x) { pure(f.call(x)) }) 
  # end
  
  # flat_map :: (a -> Future b) -> Future b
  def flat_map(f)
    Future.new(-> cb { 
      subscribe(-> (value) {
        if value.is_left
          cb.call(value) 
        else
          f.call(value.value).subscribe(cb)
        end
      }) 
    })
  end

  # traverse :: [a] -> (a -> Future b) -> Future [b]
  def self.traverse(arr, f)
    arr.reduce(Future.pure([])) do |acc, elem|
      acc.flat_map(-> values {
        f.call(elem).map(-> val { values + [val] })
      })
    end
  end

  # NOTICE
  # callback :: Either err a -> void
  # execute all the callbacks in the subsribers
  def callback(value)
    semaphore.pop
    self.cache = Some.new(value)
    while subscribers.count > 0
      sub = self.subscribers.shift
      Thread.new { sub.call(value) }
    end
    semaphore.push(nil)
  end
  
  # subscribe :: (Either err a -> void) -> void
  def subscribe(subscriber)
    semaphore.pop
    if self.cache.defined
      semaphore.push(nil)
      subscriber.call(cache.value)
    else
      self.subscribers.push(subscriber)
      semaphore.push(nil)
    end
  end
end
