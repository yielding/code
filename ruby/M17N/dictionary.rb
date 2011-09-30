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

module MD
  #
  # 아래의 String은 파일에 종속적이다. 사전에 종속적이지 않다.
  #
  class Dictionary
    attr_reader :dict

    def initialize(file="")
      @dict = {}
      unless file.empty?
        @file = file
        @dict = JSON.parse(File.read(file))
      end
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

    def update keys
      keys.each { |k| @dict[k] = ["", ""] if k.has_korean? and @dict[k].nil?  }
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
    def initialize fileinfo
      name, @params = fileinfo
      @kor, @eng, @jpn = name.split(/:/)
      @dictionary = nil
      @w = "w"
      @r = "r"
    end

    def ResourceFile.factory fileinfo
      first, _, _ = fileinfo[0].split(/:/)  # first:second:third
      class_  = { ".xml" => XMLFile, ".xaml" => XAMLFile, ".rc" => RCFile }
      return class_[File.extname(first)].new(fileinfo)
    end

    def read
      File.open(@kor, @r).read()
    end

    def save_eng text
      File.open(@eng, @w) { |f| f.write(text) }
    end

    def save_jpn text
      File.open(@jpn, @w) { |f| f.write(text) }
    end

    def with dict
      @dictionary = dict  
      self
    end

    def to_english
      text = read
      @dictionary.dict.each { |key, value| text.gsub!(key, value[0]) }
      text
    end

    def to_japanese
      text = read
      @dictionary.dict.each { |key, value| text.gsub!(key, value[1]) }
      text
    end
  end

  # 각 파일의 path 및 인코딩 처리
  class RCFile < ResourceFile
    def initialize filename
      super filename
      @w = "w:utf-16le"
      @r = "r:utf-16le:utf-8"
    end
    
    def extract_key
      keys = Set.new
      read.each_line { |it|
        if it.has_korean? 
          lines = it.extract_rc_korean
          lines.split(/\\n|\\r/).each { |line| keys.add(line) unless line.empty? }
        end
      }

      keys
    end
  end

  class XMLFile < ResourceFile
    def initialize filename 
      super filename
    end

    def extract_key
      keys = Set.new
      doc  = REXML::Document.new(File.new(@kor))
      @params.each { |k, v|
        attrs_of(doc, k, v, keys) unless v.empty?
        texts_of(doc, k, keys) if v.empty?
      }

      keys.to_a
    end

    private
    def collect_keys_from(lines, keys)
      lines.each { |l|
        l.strip!; l = l[0...-1] while l.end_with?(".", "?", "\n", "\r")
        keys.add(l) unless l.empty?
      }
    end

    def attrs_of(doc, path, key, keys)
      doc.elements.each(path) do |e|
        unless e.attributes[key].nil?
          lines = e.attributes[key].lines
          collect_keys_from(lines, keys) unless lines.nil?
        end
      end
    end

    def texts_of(doc, path, keys)
      doc.elements.each(path) do |e|
        collect_keys_from(e.text.lines, keys) unless e.text.nil?
      end
    end
  end

  class XAMLFile < ResourceFile
    def initialize filename
      super filename
    end

    def extract_key
      keys = Set.new
      read.each_line { |l|
        if l.has_korean?
          l.strip!
          keys.add(l.extract_korean)
        end
      }

      keys.to_a
    end
  end

end

################################################################################
#
#
#
################################################################################
if __FILE__ == $PROGRAM_NAME
  xamls = {
    "./CasePage.xaml:./CasePage_eng.xaml:./CasePage_jpn.xaml" => {},
    "./EvidenceInfo.xaml:./EvidenceInfo_eng.xaml:./EvidenceInfo_jpn.xaml" => {}
  }

  xmls = {
    "./Logs.xml:./Logs_eng.xml:./Logs_jpn.xml" => { 
      "Logs/Category/Log"              => "title",   # key.attribute => value
      "Logs/Category/Log/Content"      => "",        # key => text
      "Logs/Category/Log/Solution"     => ""
    },
    "./StringTable.xml:./StringTable_eng.xml:./StringTable_jpn.xml" => { 
      "StringTable/Category/String"    => "" 
    },
    "./Dialogs.xml:./Dialogs_eng.xml:./Dialogs_jpn.xml" => {
      "Dialogs/Dialog/WindowTitle"     => "",
      "Dialogs/Dialog/MainInstruction" => "",
      "Dialogs/Dialog/Content"         => "",
      "Dialogs/Dialog/Verification"    => "",
      "Dialogs/Dialog/Buttons/Button"  => "",
      "Dialogs/Dialog/Buttons/CommandLink/Text" => "",
      "Dialogs/Dialog/Buttons/CommandLink/Explanation" => ""
    }
  }

  rcs = { 
    "./Besmaster.rc:./Besmaster_eng.rc:./Besmaster_jpn.rc" => {}
  }

  #
  # begin processing
  #
  dict1 = MD::Dictionary.new("kje_dictionary.json")
  resource_files = xamls.merge(xmls).merge(rcs)
  resource_files.each { |file| 
    f = MD::ResourceFile.factory(file)
    dict1.update(f.extract_key)
  }

  dict1.print
  dict1.save

  files.each { |file|
    f = MD::ResourceFile.factory(file)
    f.save_eng(f.with(dict1).to_english)
    f.save_jpn(f.with(dict1).to_japanese)
  }
  
end
