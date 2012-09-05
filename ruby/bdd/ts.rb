#!/usr/bin/env ruby19
# encoding: utf-8

class TennisScorer
  OPPOSITE_SIDE_OF_NET = { 
    :server   => :receiver, 
    :receiver => :server 
  } 

  NEXT_SCORE = { 0 => 15, 15 => 30, 30 => 40, 40 => -1 }

  def initialize
    @score = { :server => 0, :receiver => 0 }
  end

  def score
    return "A-Server" if a_server?
    return "A-Receiver" if a_receiver?
    return "Deuce" if deuce? 
    return "W-L" if w_l?
    return "L-W" if l_w?
    return "#{@score[:server]}-#{@score[:receiver]}"
  end

  def give_point_to player
    other = OPPOSITE_SIDE_OF_NET[player]
    fail "Unknown player #{whom}" unless other
    
    @score[player] = NEXT_SCORE[@score[player]]
  end
  
  def w_l?
    @score[:server] == -1 and @score[:receiver] < 40
  end
  
  def l_w?
    @score[:server] < 40 and @score[:receiver] == -1
  end
  
  def deuce?
    @score[:server] == 40 and @score[:receiver] == 40
  end
  
  def a_server?
    @score[:server] == -1 and @score[:receiver] == 40
  end
  
  def a_receiver?
    @score[:server] == 40 and @score[:receiver] == -1
  end
end
