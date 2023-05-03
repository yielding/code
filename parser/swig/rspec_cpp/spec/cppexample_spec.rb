require File.dirname(__FILE__) + '/spec_helper'
require 'example'

describe Example::CppExample do
  it "should be a constant on module Example" do
    Example.constants.should include('CppExample')
  end
end

describe Example::CppExample, ".new" do
  it "should create a new object of type CppExample" do
    example = Example::CppExample.new("example1", 1)
    example.title.should == "example1"
    example.flag.should  == 1
  end
end

describe Example::CppExample, "#title(value)" do
  it "should set the title" do
    example = Example::CppExample.new("example1", 1)
    example.title("title2")
    example.title.should == "title2"
  end
end

describe Example::CppExample, "#flag(value)" do
  it "should set the flag" do
    example = Example::CppExample.new("example1", 1)
    example.flag(2)
    example.flag.should == 2
  end
end
