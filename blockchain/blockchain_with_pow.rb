#!/usr/bin/env ruby

require 'digest'
require 'pp'

class Block
  attr_reader :index
  attr_reader :timestamp
  attr_reader :data
  attr_reader :previous_hash
  attr_reader :nonce
  attr_reader :hash

  def initialize(index, data, previous_hash)
    @index = index
    @data = data
    @previous_hash = previous_hash
    @nonce, @hash  = compute_hash_with_proof_of_work
  end

  def compute_hash_with_proof_of_work(difficulty="00")
    nonce = 0
    loop do
      hash = calc_hash_with_nonce(nonce)
      if hash.start_with? difficulty
        return [nonce, hash]
      else
        nonce += 1
      end
    end
  end

  def calc_hash_with_nonce(nonce=0)
    sha = Digest::SHA256.new
    sha.update(nonce.to_s + @index.to_s + @timestamp.to_s + @data + @previous_hash)
    sha.hexdigest
  end

  def self.first(data="Genesis")
    Block.new(0, data, "0")
  end

  def self.next(previous, data="Transaction Data...")
    Block.new(previous.index + 1, data, previous.hash)
  end

end

b0 = Block.first("Genesis")
b1 = Block.next(b0, "TR 1")
b2 = Block.next(b1, "TR 2")
b3 = Block.next(b2, "TR 3")

blockchain = [b0, b1, b2, b3]

pp blockchain
