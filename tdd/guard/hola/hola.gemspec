# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'hola/version'

Gem::Specification.new do |s|
  s.name          = "hola"
  s.version       = "0.0.1"
  s.authors       = ["Lee Chang Ha at saturn"]
  s.email         = ["yielding@icloud.com"]

  s.summary       = %q{만세}
  s.description   = %q{A simple 안녕}
  s.homepage      = "http://rubygems.org/gems/hola"
  s.files         = ["lib/hola.rb", "lib/hola/translator.rb"]
  s.bindir        = "bin"
  s.executables   << "hola"
  s.require_paths = ["lib"]
  s.license       = "MIT"

end
