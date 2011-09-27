#!/usr/bin/env ruby -wKU

require "set"

set = Set.new
str = "a very very long string"

str.scan(/\w+/).each { |word| 
  unless set.add?(word)
    p word
  end
}

