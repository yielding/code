#!/usr/bin/env ruby
# encoding: utf-8

require "net/http"
require "uri"
require "json"

def detect_lang text
  txt = URI.escape(text)
  uri = "http://www.google.com/uds/GlangDetect?v=1.0&q=#{txt}"
  res = Net::HTTP.get_response URI.parse(uri)

  case res
  when Net::HTTPSuccess
    (JSON.load res.body)['responseData']['language']
  else
    res.error!
  end
end


p detect_lang "한글"
p detect_lang "理氣互發說"
