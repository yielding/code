#!/usr/bin/env ruby

class String
  def to_unicode
    return nil unless self.start_with?("&#")
    [self[2..-1].to_i].pack("U")
  end
end

chrs = "&#49324;&#51109;&#45784;&#48516;&#50976;&#44050;&#51060;&#47784;&#51088;&#46972;&#50836;"
puts chrs.split(/\;/).map { |ch| ch.to_unicode }
