#!/usr/bin/env ruby

class Brewer
  attr_accessor :start, :tank, :pot
  attr_accessor :boiler, :indicator

  def initialize
    @state = :idle
  end

  def trigger
    case @state
    when :idle
      if @start.pushed? && @tank.wet?
	goto_brewing
      end

    when :brewing
      if @tank.dry?
	goto_done
      elsif ! @pot.pot_present?
	goto_paused
      end

    when :paused
      if @pot.pot_present? && @tank.dry?
	goto_done
      elsif @pot.pot_present?
	goto_brewing
      end

    when :done
      if ! @pot.pot_present?
	goto_idle
      end
    end
  end

  private

  def goto_brewing
    @boiler.on
    @indicator.off
    @state = :brewing
  end

  def goto_done
    @boiler.off
    @indicator.on
    @state = :done
  end

  def goto_idle
    @boiler.off
    @indicator.off
    @state = :idle
  end

  def goto_paused
    @boiler.off
    @indicator.off
    @state = :paused
  end
end
