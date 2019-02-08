#!/usr/bin/env ruby

require 'test/unit'
require 'markiv/warmer'
require 'markiv/mocks'

class TestWarmer < Test::Unit::TestCase
  def setup
    @pot_sensor = MockPotSensor.new
    @heater = MockOnOffDevice.new
    @warmer = Warmer.new(@pot_sensor, @heater)
  end

  def test_coffee_present
    @pot_sensor.coffee_present
    @warmer.trigger
    assert @heater.on?    
  end

  def test_coffee_absent
    @pot_sensor.pot_present
    @warmer.trigger
    assert @heater.off?
  end

  def test_cycle
    @pot_sensor.coffee_present
    @warmer.trigger
    assert @heater.on?    

    @pot_sensor.pot_present
    @warmer.trigger
    assert @heater.off?

    @pot_sensor.coffee_present
    @warmer.trigger
    assert @heater.on?

    @pot_sensor.no_pot
    @warmer.trigger
    assert @heater.off?
  end
end
