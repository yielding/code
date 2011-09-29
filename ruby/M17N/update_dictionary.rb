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
  def korean?
    self.index(/\p{Hangul}/) != nil
  end

  def has_korean?
    self.korean?
  end

  def extract_rc_korean
    b = self.index(/\p{Hangul}/) # 'b' means begin
    b -= 1 while self[b] != '"'  # find "
    b += 1                       # delete "

    e = self.rindex(/\p{Hangul}/)
    e += 1 while self[e] != '"'  # find "
    e -= 1                       # delete "
    e -= 1 while ['.', ':', '?', '!', ' '].include?(self[e])

    # (b != nil) ? self[b, e - b + 1] : ""
    self[b, e - b + 1]
  end

  def extract_korean
    b, e = self.index(/\p{Hangul}/), self.rindex(/\p{Hangul}/)
    (b != nil) ? self[b, e - b + 1] : ""
  end
end

################################################################################
#
#
#
################################################################################
class MDDictionary
  TABLE = { :eng => 0, :jpn => 1 } 
  DIR   = { :asc => 1, :dsc =>-1 }

  def initialize(file)
    @file = file
    @dict = sort(JSON.parse(File.read(file)).to_a)
  end 

  def sort arr, dir=DIR[:dsc]
    arr.sort_by { |l| l[0].gsub(/\ +/, ' ').length * dir }
  end 

  def ok? 
    @dict.size > 0 
  end 

  def to_english text
    translate_utf8 text, :eng
  end

  # index = 0 -> english
  def translate_utf8 text, index
    @dict.each { |word| 
      key, eng = word[0], word[1][TABLE[index]] 
      text.gsub!(key, eng)
    }   
    text
  end

  def update_with files, encoding="utf-8"
    files.each do |file|
      mode = (encoding == "utf-8") ? "r" : "r:utf-16le:utf-8"
      File.open(file, mode).each_line { |line|
        if line.has_korean?
          key = line.extract_korean
          @dict << [key, ["", ""]] unless self.has_same(key)
        end
      }
    end
  end
  
  def update_with_rc files
    files.each do |file|
      File.open(file, "r:utf-16le:utf-8").each_line do |line|
        if line.has_korean?
          key = line.extract_rc_korean
          @dict << [key, ["", ""]] unless self.has_same(key)
        end
      end
    end
  end 

  def update_with_xml files
    files.each do |file|
      doc   = REXML::Document.new(File.new(file))
      lines = Set.new

      if file.end_with? "Logs.xml"
        attrs_of(doc, 'Logs/Category/Log', "title", lines)
        texts_of(doc, 'Logs/Category/Log/Content', lines)
        texts_of(doc, 'Logs/Category/Log/Solution', lines)
      end

      if file.end_with? "Dialogs.xml"
        texts_of(doc, 'Dialogs/Dialog/WindowTitle', lines)
        texts_of(doc, 'Dialogs/Dialog/MainInstruction', lines)
        texts_of(doc, 'Dialogs/Dialog/Content', lines)
        texts_of(doc, 'Dialogs/Dialog/Verification', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/Button', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/CommandLink/Text', lines)
        texts_of(doc, 'Dialogs/Dialog/Buttons/CommandLink/Explanation', lines)
      end

      if file.end_with? "StringTable.xml"
        texts_of(doc, 'StringTable/Category/String', lines)
      end

      lines.each { |line| 
        @dict << [line, ["", ""]] if line.has_korean? and not has_same(line)
      }
    end
  end 

  def update_with_xaml files
    # TODO
  end

  def has_same key 
    @dict.each { |item| return true if item[0] == key }
    false
  end 

  def save
    @dict = sort(@dict)
    hash = {}; @dict.each { |item| hash[item[0]] = item[1] }
    File.open(@file, "w") { |f| f.write(JSON.pretty_generate(hash)) }
  end 

  def print lang=:eng
    @dict = sort(@dict)
    hash = {}; @dict.each { |item| hash[item[0]] = item[1] }
    pp hash
  end 

  private
  def attrs_of(doc, path, key, lines)
    doc.elements.each(path) { |e| 
      unless e.attributes[key].nil?
        e.attributes[key].lines { |line|
          line.strip!
          line = line[0...-1] while line.end_with?(".", "?", "\n", "\r")
          lines.add(line) unless line.nil?
        }
      end
    }
  end

  def texts_of(doc, path, lines)
    doc.elements.each(path) { |e| 
      unless e.text.nil? 
        e.text.lines { |line| 
          line.strip!
          line = line[0...-1] while line.end_with?(".", "?", "\n", "\r")
          lines.add(line) unless line.nil?
        }
      end
    }
  end
end

################################################################################
#
#
#
################################################################################
if __FILE__ == $PROGRAM_NAME
  kor_xaml_files = %w{
    ../Bin/Resource/Kor/CasePage.xaml
    ../Bin/Resource/Kor/EvidenceInfo.xaml
    ../Bin/Resource/Kor/EvidencePage.xaml
    ../Bin/Resource/Kor/RecentItemFormat.xaml
    ../Bin/Resource/Kor/RecentTsfFormat.xaml
    ../Bin/Resource/Kor/StartPage.xaml
    ../Bin/Resource/Kor/NewCaseNotice_InsufficientSpace.xaml
  }

  kor_xml_files = %w{
    ../Besmaster/resHTML/Dialogs.xml
    ../Besmaster/resHTML/Logs.xml
    ../Besmaster/resHTML/StringTable.xml
  }

  kor_rc_files = %w{ ../Besmaster/Besmaster.rc }

  kor_setting_files = %w{
    ../Bin/Model\ Settings\ For\ Smart/_MAKER_APPLE/iPhone4.tsf
    ../Bin/Model\ Settings\ For\ Smart/_MAKER_SAMSUNG/SC-02B.tsf
    ../Bin/Model\ Settings\ For\ Smart/_MAKER_SAMSUNG/SC-02B(MOVI).tsf
  }

  dict1 = MDDictionary.new("kje_dictionary.json")
  dict1.update_with(kor_xaml_files, "utf-8")
  dict1.update_with_xml(kor_xml_files)
  dict1.update_with_rc(kor_rc_files)
  dict1.save
  #dict1.print
  
  dict2 = MDDictionary.new("settings.json")
  dict2.update_with(kor_setting_files, "utf-16le")
  dict2.save
end
