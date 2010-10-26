class Array
  def initialize_copy(other)
    other.each { |item| self.push(item.clone) }
  end
end

if __FILE__ == $0
  a = [ ["kamin", "gunhee"], ["leech", "gunhee"] ]
  p a
  a.each { |item| 
    d = item.dup
    d.shift if d[0] == "leech"
    p d
  }
  p a
end
