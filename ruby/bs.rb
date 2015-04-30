class Item
  include Comparable

  attr_reader :id, :value

  def initialize(id, val)
    @id = id
    @value = val
  end

  def <=> (other)
    res = @value <=> other.value
    return res if res != 0

    @id <=> other.id
  end

  def inspect
    "(#{id}, #{value})"
  end
end

class Array

  def bsearch2
    return nil unless block_given?
    lo, hi = 0, self.length - 1

    while lo <= hi do
      mid = lo + (hi - lo) / 2
      val = self[mid]
      res = yield(val)
      if res == -1
        lo = mid + 1
      elsif res == 1
        hi = mid - 1
      else
        return mid
      end
    end

    ~lo
  end
end

a = [Item.new(1, 6), Item.new(2, 4), Item.new(3, 3)]

a.sort!

p a

res = a.bsearch2 { |x| x <=> Item.new(4, 3) }
if res < 0
  p ~res
end
