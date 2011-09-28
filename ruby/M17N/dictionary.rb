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

  dict1 = MD::Dictionary.new("kje_dictionary.json")
  #dict1.update_with(kor_xaml_files, "utf-8")
  #dict1.update_with_xml(kor_xml_files)
  #dict1.update_with_rc(kor_rc_files)
  dict1.save
  dict1.print
  
end
