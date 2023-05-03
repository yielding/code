require File.dirname(__FILE__) + '/spec_helper'
require 'example'

describe "Example (C functions)" do
  it "should be a constant on Module" do
    Module.constants.should include('Example')
  end
  it "should have the methods defined in the C header file" do
    Example.methods.should include('returnString')
    Example.methods.should include('returnDouble')
    Example.methods.should include('doNothing')
  end
end

describe Example, ".returnString" do
  it "should return the input char * string as a Ruby string unchanged" do
    Example.returnString("bar!").should == "bar!"
  end  
end

describe Example, ".returnDouble" do
  it "should return the input integer as a double" do
    Example.returnDouble(10).should == 10.0
  end
end

describe Example, ".doNothing" do
  it "should exist, but do nothing" do
    lambda { Example.doNothing }.should_not raise_error
  end
end
