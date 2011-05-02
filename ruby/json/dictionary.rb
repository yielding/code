#!/usr/bin/env ruby -wKU
#-*- coding:utf-8 -*-

require "json"

class MDDictionary
  def initialize(file)
    @table = { :eng => 0, :jpn => 1 }
    @text  = File.read(file)
    @ok    = @text.length > 0
    @dict  = JSON.parse(@text).sort_by do |line| 
      line[0].gsub(/\ +/, ' ').length * -1 
    end
  end

  def ok?; @ok end

  def to_english text
    translate_utf8 text, :eng
  end

  # index = 0 -> english
  def translate_utf8 text, index
    @dict.each do |word| 
      key, eng = word[0], word[1][@table[index]] 
      text.gsub!(key, eng)
    end
    text
  end

  def print lang=:eng
    return unless @ok
    @dict.each { |line| printf "(%s, %s)\n", line[0], line[1][@table[lang]] }
  end
end

class KoreanFile
  def initialize(filename, dict, encoding="utf-8")
    @filename = filename
    @utf8 = (encoding == "utf-8")
    @dict = dict
  end

  def to_english
    text = read_text
    @dict.to_english(text)
  end
  
  def read_text
    mode = @utf8 ? "r" : "r:utf-16le:utf-8"
    text = File.open(@filename, mode).read()
    text
  end
  
  def save text
    mode = @utf8 ? "w" : "w:utf-16le"
    File.open(@filename, mode) { |f| f.write(text) }
  end
  
  def backup text, ext="org"
    backup_name = "#{@filename}.#{ext}"
    unless File.exists?(backup_name)
      mode = @utf8 ? "w" : "w:utf-16le"
      File.open(backup_name, mode) { |f| f.write(text) }
    end
  end
end

if __FILE__ == $PROGRAM_NAME
  xaml_files = %w{
    Eng/CasePage.xaml
    Eng/EvidenceInfo.xaml
    Eng/EvidencePage.xaml
    Eng/RecentItemFormat.xaml
    Eng/RecentTsfFormat.xaml
    Eng/StartPage.xaml
    Eng/NewCaseNotice_InsufficientSpace.xaml
  }

  xml_files = %w{
    ../../ResourceDLL/BesmasterResEng/res/Dialogs.xml
    ../../ResourceDLL/BesmasterResEng/res/Logs.xml
    ../../ResourceDLL/BesmasterResEng/res/StringTable.xml
  }

  rc_files = %w{ 
    ../../ResourceDLL/BesmasterResEng/BesmasterResEng.rc 
  }

  dict = MDDictionary.new("dictionary.json")

  utf8_files = xaml_files + xml_files
  utf8_files.each { |file| 
    f = KoreanFile.new(file, dict)
    text = f.to_english
    f.backup text
    f.save text
  }

  rc_files.each { |file| 
    f = KoreanFile.new(file, dict, "utf16-le")
    text = f.to_english
    puts text
    f.backup text
    f.save text
  }
  
end
