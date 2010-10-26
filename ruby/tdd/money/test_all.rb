#!/usr/bin/env ruby

require 'test/unit/testsuite'
require 'test/unit/ui/console/testrunner'
require 'test_money'

class TestSuiteAllTests
  def self.suite
    suite = Test::Unit::TestSuite.new("Money")
    suite << TestMoney.suite
    return suite
  end
end

Test::Unit::UI::Console::TestRunner.run(TestSuiteAllTests)
