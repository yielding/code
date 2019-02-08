#!/usr/bin/env ruby

require 'test/unit'
require 'markiv/adapter'
require 'flexmock'

class TestAdapter < Test::Unit::TestCase
  def test_messages
    @valve = MockOpenCloseDevice.new
    @adapter = Adapter.new(@valve, :on => :close, :off => :open)
    @adapter.on
    assert @valve.closed?
    @adapter.off
    assert @valve.open?
  end

  def test_inverse
    @valve = MockOpenCloseDevice.new
    @adapter = Adapter.new(@valve, :on => :open, :off => :close)
    @adapter.on
    assert @valve.open?
    @adapter.off
    assert @valve.closed?
  end

  def test_strings
    @valve = MockOpenCloseDevice.new
    @adapter = Adapter.new(@valve,
      :on => "close", :off => "open")
    @adapter.on
    assert @valve.closed?
    @adapter.off
    assert @valve.open?
  end

  def test_blocks
    @valve = MockOpenCloseDevice.new
    @adapter = Adapter.new(
      @valve,
      :on => lambda { |d| d.close },
      :off => lambda { |d| d.open }
      )
    @adapter.on
    assert @valve.closed?
    @adapter.off
    assert @valve.open?  
  end

  def test_mixed
    @valve = MockOpenCloseDevice.new
    @adapter = Adapter.new(
      @valve,
      :off => lambda { |d| d.open },
      :on => "close"
      )
    @adapter.on
    assert @valve.closed?
    @adapter.off
    assert @valve.open?  
  end
  
  def test_exception
    @valve = MockOpenCloseDevice.new
    assert_raise(RuntimeError) {
      @adapter = Adapter.new(
	@valve,
	:on => 1,
	:off => lambda { |d| d.open }
	)
    }
  end

  def test_unmapped_method_passes_through
    FlexMock.use do |m|
      called = false
      m.mock_handle(:hi) { called = true }
      adapter = Adapter.new(m)
      adapter.hi
      assert called, "called"
    end
  end
  
  def test_arguments
    FlexMock.use do |m|
      m.mock_handle(:message) do |hello, world|
	assert_equal "hello", hello
	assert_equal "world", world
      end
      adapter = Adapter.new(m, :puts => :message)
      adapter.puts("hello", "world")
    end
  end

end
