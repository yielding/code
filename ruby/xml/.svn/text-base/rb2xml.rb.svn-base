#!/usr/bin/env ruby

require 'rexml/document'
require 'pp'

include REXML

class Rb2XML
  def rb2xml(h)
    h[:infilepath] = './' if h[:infilepath].nil?
    h[:outfilepath] = './' if h[:outfilepath].nil?
    h[:xmlfile] = h[:sourcefile][/^(.*)\.\w+$/,1] + '.xml' if h[:xmlfile].nil?
    
    buffer = File.new(h[:infilepath] + h[:sourcefile], 'r').read

    doc = Document.new
    doc.add_element('rb_txt')
    body = Element.new('source')
    body.text = CData.new(buffer)
    doc.root.add_element(body)

    puts h[:outfilepath]
    puts h[:xmlfile]

    file = File.new(h[:outfilepath] + h[:xmlfile], 'w')
    file.puts doc
    file.close
  end
end

if __FILE__ == $0
  r2x = Rb2XML.new
  r2x.rb2xml(:sourcefile => 'rb2xml.rb')
end
