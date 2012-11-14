#!/usr/bin/env ruby19

require "test/unit"
require "flexmock/test_unit"

class Woofer
  def woof
    :grrr
  end
end

class Dog
  def initialize
    @woofer = Woofer.new
  end

  def bark
    @woofer.woof
  end

  def wag
    :happy
  end
end

class TestDogBarking < Test::Unit::TestCase
  include FlexMock::TestCase

  def setup
    @dog = Dog.new
    flexmock(@doc, :bark => :grrr)
  end

  def test_dog
    assert_equal(:grrr, @dog.bark)
    assert_equal(:happy, @dog.wag)
  end
end
