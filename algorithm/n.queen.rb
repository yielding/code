require "set"

a = [*0...8]
a.permutation.each { |c| 
  s = [ Set.new(c.zip(a).map { |e| e.reduce(:+) }), 
        Set.new(c.zip(a).map { |e| e.reduce(:-) }) ]
  p c if s.all? { |x| x.size == 8 }
}
