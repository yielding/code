# -*- encoding: utf-8 -*-

require_relative 'ts'

describe TennisScorer, "basic scoring" do
  before (:each) do
    @ts = TennisScorer.new
  end

  it "반드시 0-0으로 시작한다" do
  #it "should start with a socre of 0-0" do
    @ts.score.should == "0-0"
  end

  it "should be 15-0 if the server wins a point" do
    @ts.give_point_to(:server)
    @ts.score.should == "15-0"
  end
  
  it "should be 0-15 if the receiver wins a point" do
    @ts.give_point_to(:receiver)
    @ts.score.should == "0-15"
  end
  
  it "should be 15-15 after they both win a point" do
    @ts.give_point_to(:receiver)
    @ts.give_point_to(:server)
    @ts.score.should == "15-15"
  end
  
  it "should be 40-0 after the server wins three points" do
    3.times { @ts.give_point_to(:server) }
    @ts.score.should == "40-0"
  end
  
  it "should be W-L after the server wins four points" do
    4.times { @ts.give_point_to(:server) }
    @ts.score.should == "W-L"
  end
  
  it "should be L-W after the receiver wins four points" do
    4.times { @ts.give_point_to(:receiver) }
    @ts.score.should == "L-W"
  end
  
  it "should be Deuce after each wins three points" do
    3.times { @ts.give_point_to(:receiver) }
    3.times { @ts.give_point_to(:server) }
    @ts.score.should == "Deuce"
  end
  
  it "should be A-server after each wins three points and the server gets one more" do
    3.times { @ts.give_point_to(:receiver) }
    3.times { @ts.give_point_to(:server) }
    
    @ts.give_point_to(:server)
    @ts.score.should == "A-Server"
  end
  
  it "should be A-server after each wins three points and the server gets one more(reverse)" do
    3.times { @ts.give_point_to(:server) }
    3.times { @ts.give_point_to(:receiver) }
    
    @ts.give_point_to(:server)
    @ts.score.should == "A-Server"
  end
  
  it "should be A-receiver after each wins three points and the server gets one more" do
    3.times { @ts.give_point_to(:receiver) }
    3.times { @ts.give_point_to(:server) }
    
    @ts.give_point_to(:receiver)
    @ts.score.should == "A-Receiver"
  end
  
  it "should be A-receiver after each wins three points and the server gets one more(reverse)" do
    3.times { @ts.give_point_to(:server) }
    3.times { @ts.give_point_to(:receiver) }
    
    @ts.give_point_to(:receiver)
    @ts.score.should == "A-Receiver"
  end
  
end
