require 'pp'

module Enumerable
  def comprehend(&block)
    block ? map(&block).compact : self
  end
end

old_data = *(1..3) # (1..3).to_a
new_data = *(3..9)

added    = new_data.comprehend {|x| x if not old_data.include?(x) }
removed  = old_data.comprehend {|x| x if not new_data.include?(x) }
same     = new_data.comprehend {|x| x if old_data.include?(x) }
modified = new_data.comprehend {|x| x**2 if not x%2 == 0 }

pp added
pp removed
pp same
pp modified
