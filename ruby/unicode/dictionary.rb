#!/usr/bin/env ruby -wKU
#-*- coding:utf-8 -*-

require "json"

xaml_files = %w{
  Eng/CasePage.xaml
  Eng/EvidenceInfo.xaml
  Eng/EvidencePage.xaml
  Eng/RecentItemFormat.xaml
  Eng/RecentTsfFormat.xaml
  Eng/StartPage.xaml
}

xml_files = %w{
  ../../ResourceDLL/BesmasterResEng/res/Dialogs.xml
  ../../ResourceDLL/BesmasterResEng/res/Logs.xml
  ../../ResourceDLL/BesmasterResEng/res/StringTable.xml
}

rc_files = %w{ 
  ../../ResourceDLL/BesmasterResEng/BesmasterResEng.rc 
}

def process_utf16le file_name
  in_file = File.open(file_name, "r:utf-16le:utf-8")
  text = in_file.read()

  # backup
  backup = "#{file_name}.org"
  File.open(backup, "w:utf-16le") { |f| f.write(text) } unless File.exists?(backup)

  # translate
  $dict.each { |word| 
    key, eng = word[0], word[1][0] 
    text.gsub!(key, eng)
  }

  # write result
  File.open(file_name, "w:utf-16le") { |f| f.write(text) }
end

def process_utf8 file_name
  text = File.read(file_name)

  # backup
  backup = "#{file_name}.org"
  File.open(backup, "w") { |f| f.write(text) } unless File.exists?(backup)

  # translate
  $dict.each { |word| 
    key, eng = word[0], word[1][0] 
    text.gsub!(key, eng)
  }

  # write result
  File.open(file_name, "w") { |f| f.write(text) }
end

$dict_text = File.read("dictionary.json")
$dict      = JSON.parse($dict_text).sort_by { |line| line[0].length * -1 }

if __FILE__ == $PROGRAM_NAME
  xaml_files.each { |file| process_utf8 file }
  xml_files.each  { |file| process_utf8 file }
  rc_files.each   { |file| process_utf16le file  }
end

