#!/usr/bin/env ruby
# encoding: utf-8

require 'test/unit' 
require 'shoulda' 
require_relative 'ts' 

class TennisScorerTest < Test::Unit::TestCase 
  def assert_score(target) 
    assert_equal(target, @ts.score) 
  end 
  
  context "Tennis scores" do 
    setup do; @ts = TennisScorer.new end 
    
    should "start with a score of 0-0" do 
      assert_score "0-0"
    end 
    
    should "be 15-0 if the server wins a point" do 
      @ts.give_point_to(:server) 
      assert_score "15-0"
    end
     
    should "be 0-15 if the receiver wins a point" do 
      @ts.give_point_to(:receiver) 
      assert_score "0-15"
    end 
    
    should "be 15-15 after they both win a point" do 
      @ts.give_point_to(:receiver) 
      @ts.give_point_to(:server) 
      assert_score "15-15"
    end
    
    should "be 40-0 after the server wins three points" do
      3.times { @ts.give_point_to(:server) }
      assert_score "40-0"
    end
    
    should "be W-L after the server wins four points" do
      4.times { @ts.give_point_to(:server) }
      assert_score "W-L"
    end
    
    should "be L-W after the receiver wins four points" do
      4.times { @ts.give_point_to(:receiver) }
      assert_score "L-W"
    end

    should "be Deuce after each wins three points" do
      3.times { @ts.give_point_to(:receiver) }
      3.times { @ts.give_point_to(:server) }
      assert_score "Deuce"
    end

    should "be A-server after each wins three points and the server gets one more" do
      3.times { @ts.give_point_to(:receiver) }
      3.times { @ts.give_point_to(:server) }

      @ts.give_point_to(:server)
      assert_score "A-Server"
    end

    should "be A-server after each wins three points and the server gets one more(reverse)" do
      3.times { @ts.give_point_to(:server) }
      3.times { @ts.give_point_to(:receiver) }

      @ts.give_point_to(:server)
      assert_score "A-Server"
    end

    should "be A-receiver after each wins three points and the server gets one more" do
      3.times { @ts.give_point_to(:receiver) }
      3.times { @ts.give_point_to(:server) }

      @ts.give_point_to(:receiver)
      assert_score "A-Receiver"
    end

    should "be A-receiver after each wins three points and the server gets one more(reverse)" do
      3.times { @ts.give_point_to(:server) }
      3.times { @ts.give_point_to(:receiver) }

      @ts.give_point_to(:receiver)
      assert_score "A-Receiver"
    end
  end 
end 
