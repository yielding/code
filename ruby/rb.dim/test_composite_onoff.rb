#!/usr/bin/env ruby

require 'test/unit'
require 'markiv/mocks.rb'
require 'markiv/onoff_composite'

class TestCompositeOnOff < Test::Unit::TestCase
  def setup
    @composite = OnOffComposite.new
    @devices = [MockOnOffDevice.new, MockOnOffDevice.new]
    @devices.each do |d| @composite.add(d) end
  end

  def test_create
    assert @devices.all? { |d| d.off? }
  end

  def test_all_on
    @composite.off
    @composite.on
    assert @devices.all? { |d| d.on? }
  end

  def test_all_off
    @composite.on
    @composite.off
    assert @devices.all? { |d| d.off? }
  end
end
