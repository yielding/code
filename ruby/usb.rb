#!/usr/bin/env ruby

require "libusb"
require "pp"

v = LIBUSB.version

p v.micro
p v.nano
