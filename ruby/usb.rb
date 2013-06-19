#!/usr/bin/env ruby2.0

require "libusb"
require "pp"

v = LIBUSB.version

p v.micro
p v.nano
