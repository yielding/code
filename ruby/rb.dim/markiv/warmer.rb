#!/usr/bin/env ruby

class Warmer
  def initialize(sensor, heater)
    @sensor = sensor
    @heater = heater
  end

  def trigger
    if @sensor.coffee_present?
      @heater.on
    else
      @heater.off
    end
  end
end
