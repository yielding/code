#!/usr/bin/env ruby -wKU

$:.push("lib")

require "battery_low"
require "count_range"

if __FILE__ == $0
  #if ARGV.size < 1
  #  puts "USAGE ..."
  #  exit
  #end

  File.open(ARGV[0]) do |f|
    # analysis = Opentech::Problems.new(f.readlines)
    analysis = Opentech::CountRange.new(f.readlines, 1)
    analysis.report
  end
end
