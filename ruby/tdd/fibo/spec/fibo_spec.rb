require "fibo.rb"

describe Fibo do
  it "should calculate for the numbers" do
    Fibo.fibo(0).should == 0
    Fibo.fibo(1).should == 1
    Fibo.fibo(2).should == 1
  end
end
