#!/usr/bin/env ruby -wKU

require "fiddle"

class MySin < Fiddle::Closure
  def call number
    Math.sin(number)
  end
end

libm = DL.dlopen('/usr/lib/libm.dylib')

function = Fiddle::Function.new(
    libm['sin'],
    [Fiddle::TYPE_DOUBLE],
    Fiddle::TYPE_DOUBLE
)

puts function.call(90 * Math::PI / 180)
