require "nokogiri"

class MyDoc < Nokogiri::XML::SAX::Document
  def initialize
    @beg = 0
  end
  
  def start_element name, attributes = []
    if name == "body"
      @beg = 1
      puts "found a #{name}"
    end
  end
  
  def end_element name, attributes = []
    if name == "body"
      @beg = 0
      puts "close a #{name}"
    end
  end
  
  def characters chars
   
  end
  
end

parser = Nokogiri::HTML::SAX::Parser.new(MyDoc.new)
parser.parse_file("/Users/yielding/Desktop/a.htm")