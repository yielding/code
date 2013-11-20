require "test/unit"

class Bitset
  attr_reader :no

  def initialize no
    @no = no
  end

  def toggle nth 
    @no ^= 1 << nth 
  end

  def turn_on nth
    @no |= 1 << nth 
  end

  def turn_off nth
    @no &= ~(1 << nth)
  end

  def union set
    Bitset.new(@no | set.no)
  end

  def to_s
    "0b#{@no.to_s(2)}"
  end
end

class TestBitSet < Test::Unit::TestCase
  def setup
    @bs = Bitset.new(16) # 0b10000
  end

  def test_turn_on
    @bs.turn_on(nth=0); assert_equal("0b10001", @bs.to_s)
    @bs.turn_on(nth=1); assert_equal("0b10011", @bs.to_s)
  end

  def test_turn_off
    @bs.turn_off(nth=0); assert_equal("0b10000", @bs.to_s)
    @bs.turn_on (nth=2); assert_equal("0b10100", @bs.to_s)
    @bs.turn_off(nth=2); assert_equal("0b10000", @bs.to_s)
  end

  def test_toggle
    @bs.toggle(nth=0); assert_equal("0b10001", @bs.to_s)
    @bs.toggle(nth=0); assert_equal("0b10000", @bs.to_s)

    @bs.toggle(nth=1); assert_equal("0b10010", @bs.to_s)
    @bs.toggle(nth=1); assert_equal("0b10000", @bs.to_s)
  end

  def test_union
    bs1 = Bitset.new(2)
    bs2 = @bs.union(bs1)
    assert_equal("0b10010", bs2.to_s)
  end


end
