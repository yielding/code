#!/usr/bin/env ruby2.0 
#encoding: utf-8

require "digest/sha1"
require 'test/unit'

class Footer
  def initialize
  end

  def write_to(path)
  end
end

class TestLibrary < Test::Unit::TestCase
  def setup
    @sha1 = Digest::SHA1.new
  end

  def test_binzip
    contents = File.binread("/usr/bin/zip")
    @sha1 << contents
    openssl_res = `openssl dgst -sha1 '/usr/bin/zip'`
    # SHA1(/usr/bin/zip)= b678c35875dc70a311e3e3a6975e0312a9fbfc1b
    expected = openssl_res.split(' ')[1]
    actual   = @sha1.hexdigest
    assert_equal(expected, actual)
  end
end
