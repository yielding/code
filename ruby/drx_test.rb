require 'rubygems'
require 'drx'

"wizard of oz".see

Math::PI.see

Enumerable.see

s = "wizard of oz"
def s.strong
    "<strong>" + self + "!</strong>"
end
s.see
