#!/usr/bin/env ruby

require "test/unit"
require "flexmock/test_unit"

class TemperatoreSampler
  def initialize(sensor)
    @sensor = sensor 
  end

  def average_temp
    total = 3.times.collect { @sensor.read_temperature }.reduce(:+)
    total / 3.0
  end
end

class TestTemperatureSampler < Test::Unit::TestCase
  def test_sensor_can_average_three_temparature_reading
    sensor = flexmock("temp")
    sensor.should_receive(:read_temperature).times(3).and_return(10, 12, 14)

    sampler = TemperatoreSampler.new(sensor)
    assert_equal(12, sampler.average_temp)
  end
end
