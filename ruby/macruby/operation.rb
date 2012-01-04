#!/usr/bin/env macruby 

class MyOperation < NSOperation
  attr_reader :name

  def initWithName name
    init
    @name = name
    self
  end

  def main
    puts "#{name} get to work!"
  end
  
end


operation_matt = MyOperation.alloc.initWithName("Matt")

queue = NSOperationQueue.alloc.init
queue.addOperation(operation_matt)

NSRunLoop.currentRunLoop.runUntilDate(NSDate.dateWithTimeIntervalSinceNow(2.0))
