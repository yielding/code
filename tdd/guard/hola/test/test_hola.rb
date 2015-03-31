#!/usr/bin/env ruby

require 'helper'

class HolaTest < MiniTest::Test
  def test_english_hello
    assert_equal "hello world", Hola.hi("english")
  end

  def test_any_hello
    assert_equal "hello world", Hola.hi("ruby")
  end
end
