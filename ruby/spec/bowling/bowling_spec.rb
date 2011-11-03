require_relative 'bowling'

describe Bowling do
  before(:each) do
    @bowling = Bowling.new
  end

  it "should score e for gutter game" do
    20.times { @bowling.hit(0) }
    @bowling.score.should == 0
  end
end
