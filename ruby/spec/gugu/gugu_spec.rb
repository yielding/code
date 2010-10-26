#-*- encoding: utf-8 -*-
require 'gugu'

context "구구단" do
  it "5단을 잘 출력한다." do
    5.dan.should eql([5, 10, 15, 20, 25, 30, 35, 40, 45])
  end
  
  specify "5단을 잘 출력한다" do
    5.dan.should eql([5, 10, 15, 20, 25, 30, 35, 40, 45])
  end

  it "should 6단을 잘 출력한다" do
    6.dan.should == [6, 12, 18, 24, 30, 36, 42, 48, 54]
  end
end
