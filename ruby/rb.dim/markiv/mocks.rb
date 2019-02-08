#!/usr/bin/env ruby

class MockOnOffDevice
  def on
    @on = true
  end

  def off
    @on = false
  end

  def on?
    @on
  end

  def off?
    !on?
  end
end

class MockPotSensor
  def pot_present?
    @pot_present
  end

  def coffee_present?
    @coffee_present
  end
  
  def pot_present
    @coffee_present = false
    @pot_present = true
  end

  def coffee_present
    @coffee_present = true
    @pot_present = true
  end

  def no_pot
    @coffee_present = false
    @pot_present = false
  end
end

class MockButtonSensor
  def push
    @pushed = true
  end

  def pushed?
    result = @pushed
    @pushed = false
    result
  end
end

class MockWaterSensor
  def no_water
    @water = false
  end

  def water
    @water = true
  end

  def dry?
    ! @water
  end

  def wet?
    @water
  end  
end

class MockOpenCloseDevice
  def open
    @closed = false
  end

  def close
    @closed = true
  end

  def open?
    ! closed?
  end

  def closed?
    @closed
  end
end
