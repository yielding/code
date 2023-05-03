require "bowling"

describe Bowling do
  it "should score 0 for gutter game" do
    10.times { Bowling.hit(0) }
    Bowling.score.should == 0
  end
end
