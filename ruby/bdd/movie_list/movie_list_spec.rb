#!/usr/bin/env ruby
# encoding: utf-8

class MovieList
  def initialize
  end

  def empty?
    true
  end
end

describe MovieList do
  context "처음 만들어질 때" do
    it "반드시 비어 있어야 한다" do
      movie_lsit = MovieList.new
      movie_lsit.should be_empty
    end
  end
end
