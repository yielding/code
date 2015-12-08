require "fibo.rb"

describe Fibo do
  before(:each) do
    @book = double("book")
    @book.stub(:title).and_return("The RSpec Book")
  end

  it "should calculate for the numbers" do
    expect(Fibo.fibo(0)).to eq(0)
    expect(Fibo.fibo(1)).to eq(1)
    expect(Fibo.fibo(2)).to eq(1)
  end

  it "should have the book" do
    expect(@book.title).to eq("The RSpec Book")
  end

  it "spec_name" do
    die = double("die")
    die.stub(:roll).and_return(1,2,3)
    expect(die.roll).to eq(1) # => 1
    expect(die.roll).to eq(2) # => 2
    expect(die.roll).to eq(3) # => 3
    expect(die.roll).to eq(3) # => 3
    expect(die.roll).to eq(3) # => 3
  end
end
