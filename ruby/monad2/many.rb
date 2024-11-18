#!/usr/bin/env ruby

# a.k.a. list monad
require 'monads/many'

include Monads

many_strings = Many.new(['hello world', 'goodbye world'])
# <struct Monads::Many values=["hello world", "goodbye world"]>

many_results = many_strings.and_then { |string| Many.new(string.split(/ /)) }
# => #<struct Monads::Many values=["hello", "world", "goodbye", "world"]>

p many_results.values
# => ["hello", "world", "goodbye", "world"]