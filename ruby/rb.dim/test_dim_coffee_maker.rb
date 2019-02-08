#!/usr/bin/env ruby

require 'test/test_manual_coffee_maker'

require 'dim'

class TestDimCoffeeMaker < TestManualCoffeeMaker
  def create_coffee_maker
    dim = register_components
    @cm = dim.coffee_maker
    @inputs = dim
    @outputs = dim
  end

  def register_components
    dim = DIM::Container.new

    dim.register(:pot_sensor)      { MockPotSensor.new }
    dim.register(:start_button)    { MockButtonSensor.new }
    dim.register(:tank)            { MockWaterSensor.new }

    dim.register(:warmer_heater)   { MockOnOffDevice.new }
    dim.register(:boiler_heater)   { MockOnOffDevice.new }
    dim.register(:indicator_light) { MockOnOffDevice.new }
    dim.register(:relief_valve)    { MockOpenCloseDevice.new }

    dim.register(:valve_adapter) { |c|
      Adapter.new(c.relief_valve,
	:on => :close,
	:off => :open)
    }

    dim.register(:boiler) { |c|
      boiler = OnOffComposite.new
      boiler.add(c.boiler_heater)
      boiler.add(c.valve_adapter)
      boiler
    }

    dim.register(:warmer) { |c|
      Warmer.new(c.pot_sensor, c.warmer_heater)
    }

    dim.register(:brewer) { |c|
      brewer = Brewer.new
      brewer.start = c.start_button
      brewer.pot = c.pot_sensor
      brewer.tank = c.tank
      brewer.indicator = c.indicator_light
      brewer.boiler = c.boiler
      brewer
    }

    dim.register(:coffee_maker) { |c|
      cm = CoffeeMaker.new
      cm.warmer = c.warmer
      cm.brewer = c.brewer
      cm.trigger
      cm
    }
    dim
  end

end
