class Array
  def add(item)
    if item.class == Array
      item.each { |x| add x }
    else
      res = seek { |x| x <=> item }
      insert(~res, item) if res < 0
    end

    self
  end

  def del(item)
    if item.class == Array
      item.each { |x| del x }
    else
      res = seek { |x| x <=> item }
      delete_at(res) if res >= 0
    end

    self
  end

  def seek
    return nil unless block_given?

    lo, hi = 0, self.length - 1

    while lo <= hi do
      mid = lo + (hi - lo) / 2
      val = self[mid]
      res = yield(val)
      if res == -1
        lo = mid + 1
      elsif res == 1
        hi = mid -1
      else
        return mid
      end
    end
    ~lo
  end
end

if __FILE__ == $PROGRAM_NAME
  a = []
  a.add(10).add(5).add([2, 3])
  p a

  a.del(5).del(2).del([3, 10])
  p a
end
