require "digest/sha1"

class Object
  def deep_copy
    Marshal.load(Marshal.dump(self))
  end
end

class Passcode
  def initialize
    @sha1 = Digest::SHA1.new
    @passcode = {}
    prepare
  end
  
  def candidates beg
    candidates_hash = {
      0 => [1, 2, 3, 4, 5, 6, 7, 8, 9], # for start
      1 => [2, 4, 5, 6, 8], 
      2 => [1, 3, 4, 5, 6],
      3 => [2, 4, 5, 6, 8],
      4 => [1, 2, 3, 5, 7, 8, 9],
      5 => [1, 2, 3, 4, 6, 7, 8, 9],
      6 => [1, 2, 3, 5, 7, 8, 9],
      7 => [2, 4, 5, 6, 8],
      8 => [1, 3, 4, 5, 6, 7, 9],
      9 => [2, 4, 5, 6, 8]
    }
    candidates_hash[beg]
  end
  
  def process arr
    @sha1.reset
    arr.each { |e| @sha1 << (e-1).chr } 
    s = arr.reduce("") { |res, e| res + sprintf("%d", e-1) } # [1, 2, 3, 4 ] => "1234"
    e = @sha1.hexdigest.upcase
    @passcode[e] = s
    printf("%s => %s\n",e, s)
  end

  def backtrack(arr, beg, depth)
    process(arr) && return if arr.length == depth
  
    candidates(beg).each do |c| 
      unless arr.include?(c)
        backtrack(arr.deep_copy.push(c), c, depth)
      end
    end
  end
  
  def prepare
    4.upto(9) { |i| backtrack([], 0, i) }
  end
  
  def passcode key
    @passcode[key]
  end
  
end

decryptor = Passcode.new

p decryptor.passcode("59A3556203C8F6C908D6C6BCE4C5E03F6BF343E3")










