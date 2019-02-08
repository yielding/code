#!/usr/bin/env ruby

require 'test/unit'
require 'ostruct'

require 'markiv/coffee_maker'
require 'markiv/mocks'
require 'markiv/warmer'
require 'markiv/brewer'
require 'markiv/adapter'
require 'markiv/onoff_composite'

class TestManualCoffeeMaker < Test::Unit::TestCase
  def setup
    create_coffee_maker
  end

  def test_idle
    check_idle
  end

  def test_brewing
    do_brewing
    check_brewing
  end

  def test_done
    do_brewing
    @inputs.tank.no_water
    @inputs.pot_sensor.coffee_present
    @cm.trigger
    check_done
  end

  def test_back_to_idle
    do_brewing
    @inputs.tank.no_water
    @inputs.pot_sensor.coffee_present
    @cm.trigger

    @inputs.pot_sensor.no_pot
    @cm.trigger
    check_idle
  end

  def create_coffee_maker
    @inputs = OpenStruct.new
    @inputs.pot_sensor = MockPotSensor.new
    @inputs.start_button = MockButtonSensor.new
    @inputs.tank = MockWaterSensor.new

    @outputs = OpenStruct.new
    @outputs.warmer_heater = MockOnOffDevice.new
    @outputs.boiler_heater = MockOnOffDevice.new
    @outputs.indicator_light = MockOnOffDevice.new
    @outputs.relief_valve = MockOpenCloseDevice.new

    valve_adapter = Adapter.new(
      @outputs.relief_valve,
      :on => :close,
      :off => :open)

    boiler = OnOffComposite.new
    boiler.add(@outputs.boiler_heater)
    boiler.add(valve_adapter)

    warmer = Warmer.new(@inputs.pot_sensor, @outputs.warmer_heater)
    brewer = Brewer.new
    brewer.start = @inputs.start_button
    brewer.pot = @inputs.pot_sensor
    brewer.tank = @inputs.tank
    brewer.indicator = @outputs.indicator_light
    brewer.boiler = boiler

    @cm = CoffeeMaker.new
    @cm.warmer = warmer
    @cm.brewer = brewer
    @cm.trigger
  end

  private

  def check_idle
    assert @outputs.warmer_heater.off?
    assert @outputs.boiler_heater.off?
    assert @outputs.indicator_light.off?
    assert @outputs.relief_valve.open?
  end

  def do_brewing
    @inputs.pot_sensor.pot_present
    @inputs.tank.water
    @inputs.start_button.push
    @cm.trigger
  end

  def check_brewing
    assert @outputs.warmer_heater.off?
    assert @outputs.boiler_heater.on?
    assert @outputs.indicator_light.off?
    assert @outputs.relief_valve.closed?
  end

  def check_done
    assert @outputs.warmer_heater.on?
    assert @outputs.boiler_heater.off?
    assert @outputs.indicator_light.on?
    assert @outputs.relief_valve.open?
  end
end
