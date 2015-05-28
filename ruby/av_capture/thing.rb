#!/usr/bin/env ruby

require 'av_capture'

# Create a recording session
session = AVCapture::Session.new

# Find the first video capable device
dev = AVCapture.devices.find(&:video?)

# Output the camera's name
$stderr.puts dev.name

# Connect the camera to the recording session
session.run_with(dev) do |connection|

  # Capture an image and write it to $stdout
  $stdout.write connection.capture
end
