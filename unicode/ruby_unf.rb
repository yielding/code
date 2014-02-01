#!/usr/bin/env ruby

require "unf"

normalizer = UNF::Normalizer.instance

a_bunch_of_strings = [ "a", "a", "a", "a", "a", "a" ]

a_bunch_of_strings.map! { |string|
  normalizer.normalize(string, :nfc)
}

string = "ㅇㅣ ㅊㅏ"
UNF::Normalizer.normalize(string, :nfc)

puts string.to_nfc
