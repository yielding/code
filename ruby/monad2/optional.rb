#!/usr/bin/env ruby

require 'monads/optional'

include Monads

optional_string = Optional.new('hello world')
#<struct Monads::Optional value="hello world">

optional_result = optional_string.and_then { |string| Optional.new(string.upcase) }
#<struct Monads::Optional value="HELLO WORLD">

p optional_result.value
# => "HELLO WORLD"

optional_string = Optional.new(nil)
# => #<struct Monads::Optional value=nil>

optional_result = optional_string.and_then { |string| Optional.new(string.upcase) }
# => #<struct Monads::Optional value=nil>

p optional_result.value