#!/usr/bin/env ruby

require 'test/unit'
require 'markiv/mocks.rb'
require 'markiv/brewer'

class TestBrewer < Test::Unit::TestCase
  def setup
    @brewer = Brewer.new
    @brewer.indicator = (@indicator = MockOnOffDevice.new)
    @brewer.boiler = (@boiler = MockOnOffDevice.new)
    @brewer.start  = (@start = MockButtonSensor.new)
    @brewer.tank = (@tank = MockWaterSensor.new)
    @brewer.pot = (@pot = MockPotSensor.new)

    @tank.no_water
  end

  def test_create
    assert_not_nil @brewer
    check :off, :off
  end

  def test_idle
    @brewer.trigger
    check :off, :off
  end

  def test_brewing
    @start.push
    @tank.water
    @brewer.trigger
    check :on, :off
  end

  def test_start_with_dry_tank
    @start.push
    @tank.no_water
    @brewer.trigger
    check :off, :off
  end

  def test_done
    @start.push
    @tank.water
    @brewer.trigger

    @tank.no_water
    @brewer.trigger
    check :off, :on
  end

  def test_back_to_idle
    @start.push
    @tank.water
    @brewer.trigger

    @tank.no_water
    @brewer.trigger
    check :off, :on

    @pot.no_pot
    @brewer.trigger
    check :off, :off
  end

  def test_pause_and_resume
    @start.push
    @tank.water
    @brewer.trigger

    @pot.no_pot
    @brewer.trigger
    check :off, :off

    @pot.coffee_present
    @brewer.trigger
    check :on, :off
  end

  def test_dry_tank_while_paused
    @start.push
    @tank.water
    @brewer.trigger

    @pot.no_pot
    @brewer.trigger
    check :off, :off

    @tank.no_water
    check :off, :off

    @pot.coffee_present
    @brewer.trigger
    check :off, :on
  end

  def check(expected_boiler, expected_indicator)
    bstate = @boiler.on? ? :on : :off
    istate = @indicator.on? ? :on : :off
    assert_equal expected_boiler, bstate, "boiler"
    assert_equal expected_indicator, istate, "indicator"
  end
end
