#!/usr/bin/env ruby -KU
#-*- coding:utf-8 -*-

require "pp"
require "set"
require "json"
require 'rexml/document'

################################################################################
#
#
#
################################################################################
module MD
  #
  # 아래의 String은 파일에 종속적이다. 사전에 종속적이지 않다.
  #
  class String
    def has_korean?
      self.index(/\p{Hangul}/) != nil
    end

    def extract_rc_korean
      b = self.index(/\p{Hangul}/) # 'b' means begin
      b -= 1 while self[b] != '"'  # find "
      b += 1                       # delete "

      e = self.rindex(/\p{Hangul}/)
      e += 1 while self[e] != '"'  # find "
      e -= 1                       # delete "
      e -= 1 while ['.', ':', '?', '!', ' '].include?(self[e])

      self[b, e - b + 1]
    end

    def extract_korean
      b, e = self.index(/\p{Hangul}/), self.rindex(/\p{Hangul}/)
      (b != nil) ? self[b, e - b + 1] : ""
    end
  end

  class Dictionary
    def initialize(file)
      @file = file
      @dict = JSON.parse(File.read(file))
    end 

    def save
      sorted = sort_by_length
      File.open(@file, "w") { |f| f.write(JSON.pretty_generate(sorted)) }
    end

    def print
      pp sort_by_length
    end

    def japanese(key)
      @dict[key][1]
    end

    def english(key)
      @dict[key][0]
    end

    # think the input string is utf-8 concatenated lines of string
    def update lines
      lines.each_line do |line|
        if line.has_korean?
          key = line.extract_korean
          @dict[key] = ["", ""] if @dict[key].nil?
        end
      end
    end

    private
    def sort_by_length
      arr = @dict.to_a.sort_by { |item| -1 * item[0].gsub(/\ +/, ' ').length }
      h = {}; arr.each { |item| h[item[0]] = item[1] }
      h
    end
  end

  # 공통 처리
  class ResourceFile
    def initialize name
      @w = "w"
      @r = "r"
      @filename = name
    end

    def read
      File.open(@filename, @r).read()
    end

    def write
    end

    def to_english
    end

    def to_japanese
    end
  end

  # 각 파일의 path 및 인코딩 처리
  class RCFile < ResourceFile
    def initialize filename
      super filename
      @w = "w:utf-16le"
      @r = "r:utf-16le:utr-8"
    end
    
    def extract_key
      hash = Hash.new
      read.each_line do |l|
        if l.has_korean?
          key = l.extract_rc_korean
          hash[key] = ["", ""] if hash[key].nil?
        end
      end
      hash
    end
  end

  class XMLFile < ResourceFile
    def initialize filename
      super filename
      @params =
    end

    def extract_key
      doc = REXML::Document.new(File.new(@filename))
    end

    private
    def collect_keys_from lines, keys
      lines.each do |line|
        line.strip!
        line = line[0...-1] while line.end_with?(".", "?", "\n", "\r")
        keys.add(line) unless line.nil?
      end
    end

    def attrs_of(doc, path, key, keys)
      doc.elements.each(path) { |e| 
        lines = e.attributes[key].lines
        collect_keys_from(lines, keys) unless lines.nil?
      }
    end

    def texts_of(doc, path, keys)
      doc.elements.each(path) { |e| 
        collect_keys_from(e.text.lines, keys) unless e.text.nil?
      }
    end
  end

  class XAMLFile < ResourceFile
    def initialize
      super
    end

    def extract_key
      text = read
    end
  end

end

################################################################################
#
#
#
################################################################################
if __FILE__ == $PROGRAM_NAME
  kor_xaml_files = %w{
    ./CasePage.xaml
    ./EvidenceInfo.xaml
  }

=begin
        texts_of(doc, 'Dialogs/Dialog/WindowTitle', lines)
        texts_of(doc, 'Dialogs/Dialog/MainInstruction', lines)
        texts_of(doc, 'Dialogs/Dialog/Content', lines)
        texts_of(doc, 'Dialogs/Dialog/Verification', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/Button', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/CommandLink/Text', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/CommandLink/Explanation', lines)
  {
    "./Logs.xml" => {"Logs/Category/Log" => "title",
                     "Logs/Category/Log/Content" => ""
                    },
  }


=end
  kor_xml_files = %w{
    ./Dialogs.xml
    ./Logs.xml
    ./StringTable.xml
  }

  kor_rc_files = %w{ ./Besmaster.rc }

  dict1 = MD::Dictionary.new("kje_dictionary.json")
  #dict1.update_with(kor_xaml_files, "utf-8")
  #dict1.update_with_xml(kor_xml_files)
  #dict1.update_with_rc(kor_rc_files)
  dict1.save
  dict1.print
  
end
