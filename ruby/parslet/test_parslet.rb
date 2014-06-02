#!/usr/bin/env ruby
#encoding: utf-8

require "test/unit"

require "parslet"
require "pp"

class SimpleParser < Parslet::Parser
  rule(:foo_rule) { str('foo').repeat }
  rule(:a_rule)   { str('simple_parser') }
  root(:a_rule)
end

class TestParslet < Test::Unit::TestCase
  def test_case_name
    assert_equal(1, 1)
    # pp SimpleParser.new.parse('simple_parser')
    # pp SimpleParser.new.root.parse('simple_parser')
    # pp SimpleParser.new.foo_rule.parse('foofoofoo')
    # pp SimpleParser.new.a_rule.parse('simple_parser')
  end
end

