#!/usr/bin/env ruby

require 'pathname'

# Access the full path using ZED_FILE
full_path = ENV["ZED_FILE"]

pn = Pathname.new(full_path)
file_name = File.basename(full_path, ".*")
exe_name  = "#{File.dirname(full_path)}/#{file_name}"

puts "[running #{file_name}]"

cmd = 
  case File.extname(full_path)
  when ".cpp"; "g++\-14 #{full_path} -o #{exe_name} && #{exe_name};"
  when  ".py"; "python3 #{full_path}"
  when  ".rb"; "ruby #{full_path}"
  when  ".rs"; "cargo run --manifest-path #{pn.parent.parent.to_path}/Cargo.toml"
  else       ; "no"
  end

puts `#{cmd}` unless cmd == "no"