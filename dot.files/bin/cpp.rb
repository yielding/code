#!/usr/bin/env ruby

require 'fileutils'

ENV_FILES = %w[.clang-format .clang-tidy .nvim.lua]
TPL_DIR   = File.expand_path("~/bin/cpp.clang++")

if ARGV.empty? || %w[--help -h].include?(ARGV[0])
  puts "Usage: cpp.rb <project_name>"
  puts "       cpp.rb --env|-e"
  puts "       cpp.rb --help|-h"
  puts
  puts "C++ project scaffolding tool (clang++, CMake, C++26)"
  puts
  puts "Commands:"
  puts "  <project_name>  Create a new C++ project from template (#{TPL_DIR})"
  puts "                  Includes: CMakeLists.txt, main.cpp"
  puts "  --env, -e       Copy env files (#{ENV_FILES.join(', ')}) to current directory"
  puts "  --help, -h      Show this help message"
  exit ARGV.empty? ? 1 : 0
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
