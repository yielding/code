#!/usr/bin/env ruby

require "set"
require "pp"

h = {}

h["이창하11"] = ["leech1", "Jpn"] 
h["민경아"]   = ["leech", "Jpn"]
h["민경아2"]  = ["leech", "Jpn"]

p h.sort_by {|key, value| - key.length }

