#!/usr/bin/env ruby19

require "minitest/spec"
require "minitest/autorun"

describe "Awesome" do
  describe 'Class' do
    it "discovers something AMAZING" do
      (10 + 10).must_equal 20
    end

    it "matches something AMAZING" do
      "vuvuzela".must_match(/vuvu/)
    end

    it "raises something AMAZING" do
      -> { raise }.must_raise(RuntimeError)
    end
  end
end

