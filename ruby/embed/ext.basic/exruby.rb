require 'test/unit' 
require 'my_test' 

class TestTest < Test::Unit::TestCase 
  def test_test 
    t = MyTest.new 
    assert_equal(Object, MyTest.superclass) 
    assert_equal(MyTest, t.class) 
    t.add(1) 
    t.add(2) 
    assert_equal([1,2], t.instance_eval("@arr")) 
  end 
end 
