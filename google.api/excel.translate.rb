#!/usr/bin/env ruby

require 'google/cloud/translate/v2'
require 'roo'

# Initialize the Google Translate API client

ENV["TRANSLATE_CREDENTIALS"] = "/Users/yielding/Desktop/a.json"

#translate = Google::Cloud::Translate.translation_service
translate = Google::Cloud::Translate::V2.new

# Specify the source and target languages
source_language = 'en'   # Example: English
target_language = 'ko'   # Example: French fr, la

# Load the Excel file
excel_file = Roo::Excelx.new('/Users/yielding/code/google.api/a.xlsx')

# Iterate over each sheet in the Excel file
excel_file.sheets.each do |sheet_name|
  # Load the sheet
  excel_file.sheet(sheet_name)

  # Iterate over each row in the sheet
  (1..excel_file.last_row).each do |row_number|
    # Get the value in the first column of the current row
    text_to_translate = excel_file.cell(row_number, 1)

    # Translate the text
    #translation = translate.translate_text(text_to_translate, from: source_language, to: target_language)
    # translation = translate.translate_text(text_to_translate, from: source_language, to: target_language)
    translation = translate.translate text_to_translate, to: target_language

    # Print the translated text
    puts "Original: #{text_to_translate}"
    puts "Translation: #{translation.text}"
    puts "--------"
  end
end