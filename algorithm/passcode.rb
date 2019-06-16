#!/usr/bin/env ruby

require "deep_clone"
require "digest/sha1"


class Passcode
  def initialize
    @sha1 = Digest::SHA1.new
    @passcode = {}
    @candidates_hash = {
      0 => [1, 2, 3, 4, 5, 6, 7, 8, 9], # for start

      1 => [2, 4, 5], 
      2 => [1, 3, 4, 5, 6],
      3 => [2, 5, 6],
      4 => [1, 2, 5, 7, 8],
      5 => [1, 2, 3, 4, 6, 7, 8, 9],
      6 => [2, 3, 5, 8, 9],
      7 => [4, 5, 8],
      8 => [4, 5, 6, 7, 9],
      9 => [5, 6, 8]
    }

    prepare_rainbow_table
  end
  
  def passcode_of hash
    @passcode[hash]
  end

private  
  def prepare_rainbow_table
    4.upto(9) { |i| backtrack([], 0, i) }
  end
  
  def process arr
    @sha1.reset
    arr.each { |e| @sha1 << (e-1).chr } 
    s = arr.map(&:to_s).reduce(:+) # [1, 2, 3, 4] => "1234"
    e = @sha1.hexdigest.upcase
    @passcode[e] = s
    printf("%s => %s\n",e, s)
  end

  def backtrack(arr, beg, depth)
    process(arr) && return if arr.length == depth
  
    @candidates_hash[beg].each do |c| 
      unless arr.include?(c)
        backtrack(DeepClone.clone(arr).push(c), c, depth)
      end
    end 
  end
end

decryptor = Passcode.new
p decryptor.passcode_of("59A3556203C8F6C908D6C6BCE4C5E03F6BF343E3")
