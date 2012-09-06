require "set"

n = 8
a = [*0...8]
a.permutation.each { |cd| 
  s = [ Set.new(cd.zip(a).map { |e| e.reduce(:+) }),
        Set.new(cd.zip(a).map { |e| e.reduce(:-) }) ]

  p cd if s.all? { |x| x.size == n }
}
