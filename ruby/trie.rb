#!/usr/bin/env ruby
# encoding: utf-8

class Trie
  def initialize
    @trie = Hash.new()
  end
 
  def build(str) 
    node = @trie    
    str.each_char do |ch| # leech
      prev = node
      node = node[ch]
      if node == nil 
        prev[ch] = Hash.new()
        node = prev[ch]
      end
    end
  end
 
  def find(str) 
    node = @trie
    str.each_char do |ch|
      node = node[ch]
      return false if node.nil?
    end
    true 
  end
end

if __FILE__ == $PROGRAM_NAME
  t = Trie.new
  t.build("leech")
  t.build("kamin")

  p t.find("kamin")
  p t.find("leeech")
  p t.find("leech")
end
