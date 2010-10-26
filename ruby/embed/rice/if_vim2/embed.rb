require 'pp'

require 'test/unit'

include VIM
 
class TestVim < Test::Unit::TestCase
  def setup
  end
  
  def test_const
    assert_equal(VIM::VIM_MAJOR, 7)
    assert_equal(VIM::VIM_MINOR, 2)
  end
  
  def test_module_function
    assert_equal(VIM.desc, "leech")
    assert_equal(VIM.set_option("set nu"), "called set nu")
    assert_equal(VIM::set_option("set lsp=5"), "called set lsp=5")
  end
   
  def test_exception
    assert_raise(RuntimeError) { VIM::set_option("set exception") }
  end
  
  def test_buffer
    assert_equal(VIM::Buffer.count, 10)
    assert_not_nil(VIM::Buffer.current)
    0.upto(9) { |i| assert_not_nil(VIM::Buffer[i]) }
    
  end
   
  def test_window
    assert_equal(VIM::Window.count, 5)
    assert_not_nil(VIM::Window.current)
    w = VIM::Window.current
    assert_equal(w.height, 80)
    assert_equal(w.width, 40)
    w.height += 20
    assert_equal(w.height, 100)
    assert_equal(w.cursor, [10, 20])
    w.cursor = [100, 200]
    assert_equal(w.cursor, [100, 200])
  end
  
end
