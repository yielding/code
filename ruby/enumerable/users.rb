#!/usr/bin/env ruby

class Users
  include Enumerable

  def initialize
    @users = %w[John Mehdi Henry]
  end
  
  def each(&block)
    for user in @users do
      yield user
    end
  end
end

p Users.new.map { |user| user.upcase }