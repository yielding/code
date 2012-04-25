#!/usr/local/bin/macruby

framework "Foundation"

class Future
  def initialize(&block)
    Thread.current[:futures] ||= Dispatch::Queue.new("org.macruby.futures-#{Thread.current.object_id}")
    @group = Dispatch::Group.new
    Thread.current[:futures].async(@group) { @value = block.call }
  end

  def value
    @group.wait
    @value
  end
end

r0 = Future.new do
  p "Engaging delayed r0 computation!"
  sleep 3.6
  30
end

r1 = Future.new do
  p "Engaging delayed r1 computation!"
  sleep 0.6
  42
end

puts r1.value
puts r0.value
