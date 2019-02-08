
#!/usr/bin/env ruby

class OnOffComposite
  def initialize
    @devices = []
  end

  def add(device)
    @devices << device
  end

  def on
    @devices.each do |d| d.on end
  end

  def off
    @devices.each do |d| d.off end
  end
end
