#!/usr/bin/env ruby

require 'rx'

stream = Rx::Observable.just(42)
stream.subscribe { |no| puts "We received: #{no}" }
