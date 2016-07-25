#!/usr/bin/env ruby

require 'googlecharts'

chart = Gchart.new(
  :type  => 'line',
  :title => "example title",
  :theme => :keynote,
  :data  => [[17, 17, 11, 8, 2],[10, 20, 15, 5, 7],[2, 3, 7, 9, 12]], 
  :line_colors => 'e0440e,e62ae5,287eec',
  :legend => ['courbe 1','courbe 2','courbe 3'],
  :axis_with_labels => ['x', 'y'], 
  :axis_range => [[0,100,20], [0,20,5]],
  :filename   => "chart.png")

# Record file in filesystem
chart.file

line = Gchart.line(
  :data => [300, 100, 30, 200, 100, 200, 300, 10], 
  :axis_with_labels => 'x',
  :axis_labels => ['Jan','July','Jan','July','Jan'],
  :filename    => "line.png")

line.file
