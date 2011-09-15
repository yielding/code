require 'test/unit'

class TestSum < Test::Unit::TestCase
  def setup
  end

  def test_major
    assert_equal VIM::VIM_MAJOR, 7
    assert_not_equal VIM::VIM_MAJOR, 3
  end

  def test_minor
    assert_equal VIM::VIM_MINOR, 2
  end

  def test_desc
    assert_equal(VIM::desc, "leech")
  end

  def test_window
    assert_equal(VIM::Window::count, 5)
    assert_equal(VIM::Window::current.width, 40)
    w = VIM::Window[2];
    assert_equal(w.width, 40)
  end
  
  def test_true
    assert_equal 1, 1
  end
end

#Test::Unit::AutoRunner.run
