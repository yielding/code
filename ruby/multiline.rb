#!/usr/bin/env ruby


def msg2
   "My name is leech
    what do you want to do?
   ".gsub(/^( |\t)+/, "")
end

messgae = <<-EOF.gsub /^( |\t)+/, ""
  My name is Lee Chang Ha
EOF


puts messgae
puts msg2
