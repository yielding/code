#!/usr/local/bin/macruby

framework 'Foundation'

class Player
  attr_accessor :points
end

leech = Player.new
leech.points = 100
p leech.valueForKey('points')
p leech.points

leech.setValue(442, forKey: 'points')
p leech.points

class Me < Hash
  def initialize
    super
    self["siblings"] = { "brothers" => ["Arnaud"], "sister" => ["yunjeong"] }
  end
end

moi = Me.new
p moi.valueForKey('siblings')
p moi.valueForKeyPath("siblings.brothers")
