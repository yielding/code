
require "pry"
require "pry-nav"
require "pry-stack_explorer"


def outer(message, number)
  inner(message)
end


def inner(message)
  local = true
  binding.pry
end

outer("hello", 100)
