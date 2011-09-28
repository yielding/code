#!/usr/bin/env ruby -KU
#-*- coding:utf-8 -*-

require "json"

class MDDictionary
  TABLE = { :eng => 0, :jpn => 1 } 
  DIR   = { :asc => 1, :dsc =>-1 }

  def initialize(file)
    @table = { :eng => 0, :jpn => 1 }
    @file = file
    @dict = sort(JSON.parse(File.read(file)).to_a)
  end 

  def sort arr, dir=DIR[:dsc]
    arr.sort_by { |l| l[0].gsub(/\ +/, ' ').length * dir }
  end 

  #def initialize(file)
  #  @table = { :eng => 0, :jpn => 1 }
  #  @text  = File.read(file)
  #  @ok    = @text.length > 0
  #  @dict  = JSON.parse(@text).sort_by do |line| 
  #    line[0].gsub(/\ +/, ' ').length * -1 
  #  end
  #end

  # index = 0 -> english
  def translate_utf8 text, index
    @dict.each do |word| 
      key, lang = word[0], word[1][@table[index]] 
      text.gsub!(key, lang)
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
    @dict.translate_utf8(text, :eng)
  end

  def to_japanese
    text = read_text
    @dict.translate_utf8(text, :jpn)
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
  
  def save_to text, path
    mode = @utf8 ? "w" : "w:utf-16le"
    File.open(path, mode) { |f| f.write(text) }
  end

  def save_test text, path
    puts text
    puts "path: #{path}"
  end

  def encoding
    return "utf-8"    if @utf8
    return "utf-16le" unless @utf8
  end
end

def translate_xaml dict, srcs, lang
  srcs.each { |file| 
    f = KoreanFile.new(file, dict)
    text = lang == :eng ? f.to_english : f.to_japanese

    paths = file.split("/")
    paths[3] = "Eng" if lang == :eng
    paths[3] = "Jpn" if lang == :jpn
    path = paths.join("/")

    f.save_to text, path
  }
end

def translate_xml dict, srcs, lang
  srcs.each { |file| 
    f = KoreanFile.new(file, dict)
    text = lang == :eng ? f.to_english : f.to_japanese;
    name = file.split("/")[-1]
    path = lang == :eng ? "../ResourceDLL/BesmasterResEng/resHTML/#{name}"
                        : "../ResourceDLL/BesmasterResJpn/resHTML/#{name}";
    f.save_to text, path
  }
end

def translate_rc dict, srcs, lang
  srcs.each { |file| 
    f = KoreanFile.new(file, dict, "utf-16le")
    text = lang == :eng ? f.to_english : f.to_japanese 
    path = lang == :eng ? "../ResourceDLL/BesmasterResEng/BesmasterResEng.rc"
                        : "../ResourceDLL/BesmasterResJpn/BesmasterResJpn.rc"
    f.save_to text, path
  }
end

def translate_setting dict, srcs, lang
  srcs.each { |file| 
    dir = file.split("/")[-2]
    f = KoreanFile.new(file, dict, "utf-16le")
    text = lang == :eng ? f.to_english : f.to_japanese 
    name = file.split(%r{/|\.})[-2]
    path = lang == :eng ? "../Bin/Model\ Settings\ For\ Smart/#{dir}/#{name}_Eng.tsf"
                        : "../Bin/Model\ Settings\ For\ Smart/#{dir}/#{name}_Jpn.tsf"
    f.save_to text, path
  }
end

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
  dict2 = MDDictionary.new("settings.json")

  translate_xaml dict1, kor_xaml_files, :eng
  translate_xaml dict1, kor_xaml_files, :jpn
 
  translate_xml dict1, kor_xml_files, :eng
  translate_xml dict1, kor_xml_files, :jpn
 
  translate_rc dict1, kor_rc_files, :eng
  translate_rc dict1, kor_rc_files, :jpn
  
  translate_setting dict2, kor_setting_files, :eng
  translate_setting dict2, kor_setting_files, :jpn

end
