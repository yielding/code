#!/usr/bin/env ruby -wKU

require_relative 'test'
blah = MyModule::MyClass.new 'a machine'
blah.greet ["Nick", "Wim", "everyone"]
