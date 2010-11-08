class String 
  def has_problem?
    (self.data? && self.has_prob_mark?) ? true : false
  end
  
  def has_prob_mark?
    self =~ /\|[:;?<>=]\|/
  end
  
  def data?
    self.length > 36
  end
  
  def link_message?
    self.length == 35
  end
  
  def wrong?
    self.length < 35 or self.length > 39
  end
  
  def afternoon?
    afternoon = 12..18
    now = self.slice(1, 2).to_i
    afternoon.include?(now)
  end
  
  def morning?
    morning = 6...11
    now = self.slice(1, 2).to_i
    morning.include?(now)
  end
end
