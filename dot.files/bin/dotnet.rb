#!/usr/bin/env ruby

require "open3"

if __FILE__ == $PROGRAM_NAME
  if ARGV.length < 1
    puts "usage: #{$PROGRAM_NAME} project_name"
    exit
  end

  filename = "#{ARGV[0]}.csproj"

  unless File.exist? filename
    File.open(filename, "w") { |file|
      file.puts '<Project Sdk="Microsoft.NET.Sdk">'
      file.puts '  <PropertyGroup>'
      file.puts '    <OutputType>Exe</OutputType>'
      file.puts '    <TargetFramework>net8.0</TargetFramework>'
      file.puts '    <ImplicitUsings>enable</ImplicitUsings>'
      file.puts '    <Nullable>enable</Nullable>'
      file.puts '  </PropertyGroup>'
      file.puts '</Project>'
    }
  end

  stdout, stderr, status = Open3.capture3("dotnet run")

  lines = stderr.split("\n")
  if lines.length > 0
    puts "[errors]"
    lines.each { |line| puts line }
  end

  lines = stdout.split("\n")
  if lines.length > 0
    puts "[output]"
    lines.each { |line| puts line }
  end
end