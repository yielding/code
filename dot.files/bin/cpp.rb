#!/usr/bin/env ruby

require 'fileutils'

ENV_FILES = %w[.clang-format .clang-tidy .nvim.lua]
TPL_DIR   = File.expand_path("~/bin/cpp.clang++")

if ARGV.empty?
  puts "Usage: cpp.rb <project_name>"
  puts "       cpp.rb --env|-e"
  exit 1
end

if %w[--env -e].include?(ARGV[0])
  ENV_FILES.each do |f|
    src = File.join(TPL_DIR, f)
    if File.exist?(src)
      FileUtils.cp(src, ".")
      puts "  #{f}"
    end
  end
else
  `cp -r #{TPL_DIR} ./#{ARGV[0]}`
end
