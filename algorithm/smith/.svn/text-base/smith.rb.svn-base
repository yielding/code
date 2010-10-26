# require 'test/unit'
require 'pp'

class Smith
  def gen n
    @res = []
    @res << n
    until n == 1
      n = n.modulo(2) == 0 ? n.div(2) : n*3 + 1
      @res << n
    end
    @res
  end
end

# class TestSmith < Test::Unit::TestCase
#   def setup
#     @s = Smith.new
#   end
# 
#   def test_modulo_div
#     assert_equal 4.modulo(2), 0
#     assert_equal 2.modulo(2), 0
#     assert_equal 4.div(2), 2
#     assert_equal 2.div(2), 1
#   end
# 
#   def test_gen
#     assert_equal @s.gen(1),  [1]
#     assert_equal @s.gen(2),  [2, 1]
#     assert_equal @s.gen(3),  [3, 10, 5, 16, 8, 4, 2, 1]
#     assert_equal @s.gen(22), [22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2, 1]
#     assert_equal @s.gen(22).length, 16
#   end
# end

s = Smith.new
res = []
for i in 900..1000 do
  res << s.gen(i).length
end

puts res.max
