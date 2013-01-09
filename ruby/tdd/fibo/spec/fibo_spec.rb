require "fibo.rb"

describe Fibo do
  before(:each) do
    @book = double("book")
    @book.stub(:title).and_return("The RSpec Book")
  end

  it "should calculate for the numbers" do
    Fibo.fibo(0).should == 0
    Fibo.fibo(1).should == 1
    Fibo.fibo(2).should == 1
  end

  it "should have the book" do
    @book.title.should == "The RSpec Book"
  end

  it "spec_name" do
    die = double("die")
    die.stub(:roll).and_return(1,2,3)
    die.roll.should == 1 # => 1
    die.roll.should == 2 # => 2
    die.roll.should == 3 # => 3
    die.roll.should == 3 # => 3
    die.roll.should == 3 # => 3
  end
end
