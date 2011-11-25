#!/usr/bin/env ruby

require "net/http"
require "nokogiri"

a1 = "http://kr.dictionary.search.yahoo.com"
a2 = "/search/dictionaryp?prop=&subtype=eng&target=&p=apple"

url = URI.parse(a1)
res = Net::HTTP.start(url.host, url.port) { |http|
  http.get(a2)
}

# puts res.body

doc = Nokogiri::XML(res.body, nil, 'UTF-8')
puts doc
