#!/usr/bin/env ruby

require "helper"

class TestHello < Minitest::Test
  def setup
    @hello = Hello.new("hello")
  end

  def test_env
    assert_equal('test', ENV["SQLITE_ENV"])
  end

  def test_message
    assert_equal(@hello.message, "hello hello")
  end
end
