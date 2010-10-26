# -*- encoding: utf-8 -*-
require "cron"
require "pp"

describe Cron, "basic parsing" do
  before(:each) do
    @cron = Cron.new "* * * * * test.rb"
  end
  
  it "should be match 0-59 if * is first"
  
end