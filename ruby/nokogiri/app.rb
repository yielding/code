require "nokogiri"
f = File.open("/Users/yielding/Desktop/shw.htm")
doc = Nokogiri::HTML(f)
p doc
f.close
