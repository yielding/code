#!/usr/bin/env ruby2.0

people = {
  :fred => { :name => "Fred", :age => 23},
  :joan => { :name => "Joan", :age => 18},
  :pete => { :name => "Pete", :age => 54}
}

people.sort_by { |k, v| v[:age] }.each { |p| p p }
