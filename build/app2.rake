#-*- ruby -*-   

require 'rake/clean'

if not defined? APP
  puts "first define APP"
  exit
end

#$APP_TEST = APP_TEST
#$APP_TEST = "#{APP}_TEST" if not defined? APP_TEST

if not defined? SRCS
  puts "define SRCS"
  exit
end

$CXX = "g++"
if defined? CXX
  $CXX = "clang++ "                 if CXX =~ /clang\+\+/
  $CXX = "g++-mp-4.6 -std=gnu++0x " if CXX =~ /c\+\+0x/
end

$CXXFLAGS=" -DPOSIX -Wall "
if defined? CXXFLAGS
  $CXXFLAGS += " -g " if CXXFLAGS == :debug
  $CXXFLAGS += " -O3 " if CXXFLAGS == :nodebug
else
  $CXXFLAGS += " -g "
end

$INCS     = " -I/opt/local/include "
$INCS    += INCS if defined? INCS

$LDFLAGS  = " -L. -L/opt/local/lib "
$LDFLAGS += LDFLAGS if defined? LDFLAGS

BOOST = {
  :t => " -lboost_thread-mt ",
  :s => " -lboost_system-mt ",
  :f => " -lboost_filesystem-mt ",
  :d => " -lboost_date_time-mt "
}

$LIBS  = ""
if defined? LIBS
  arr = LIBS.split
  $LIBS += BOOST[:t] if arr.include?(":t")
  $LIBS += BOOST[:s] if arr.include?(":s")
  $LIBS += BOOST[:f] if arr.include?(":f")
  $LIBS += BOOST[:d] if arr.include?(":d")
  arr.each { |e| $LIBS += sprintf(" %s") if e =~/^-l/ }
end

CLEAN.include("*.o")
CLOBBER.include(APP).include(APP_TEST).include("*.exe")

#------------------------------------------------------------------------------
#
# 
#
#------------------------------------------------------------------------------
class String
  def osx; self + '.osx'; end
  def o;   self + '.o';   end
  def cpp; self;          end
  def obj; self + '.obj'; end
end

class Builder
  attr_reader :os
  def initialize opt
    @os = opt[:os]
  end

  def os_o obj
    case os
    when :osx
      obj.osx.o
    end
  end

  def should_compile? obj
    if File.exist? os_o(obj)
      src_time = File.mtime obj.cpp
      obj_time = File.mtime os_o(obj)
      src_time > obj_time
    else
      true
    end
  end

  def should_link? bin, objs 
    if File.exist? bin 
      mtime = File.mtime bin 
      objs.any? { |obj| File.mtime(os_o(obj)) > mtime }
    else
      true
    end
  end

  def compile objs=[] 
    objs.each do |obj|
      if should_compile? obj
        case os
        when :osx
          sh "#{$CXX} -c #{$CXXFLAGS} #{$INCS} #{obj.cpp} -o #{obj.osx.o}" 
        end
      end
    end
  end

  def link app, objs 
    objs_os_o = objs.map{|obj| os_o(obj)}.join ' '
    if should_link? app, objs
      case os
      when :osx
        sh "#{$CXX} -o #{app} #{objs_os_o} #{$LDFLAGS} #{$LIBS}"
      end
    end
  end
end

#------------------------------------------------------------------------------
#
# Compile and Link target
#
#------------------------------------------------------------------------------
task :osx_compile do
  builder = Builder.new :os => :osx
  builder.compile SRCS
end

task :osx_test_compile do
  builder = Builder.new :os => :osx
  builder.compile TEST_SRCS
end

task :osx_link => [:osx_compile] do
  builder = Builder.new :os => :osx
  builder.link APP, SRCS
end

task :osx_tets_link => [:osx_test_compile] do
  builder = Builder.new :os => :osx
  builder.link APP_TEST, TEST_SRCS
end

#------------------------------------------------------------------------------
#
# Build target
#
#------------------------------------------------------------------------------
task :default => [:osx]

task :osx => [:osx_link] do
  puts "#{APP} build ok"
end

task :osx_test => [:osx_test_link] do
  puts "#{APP_TEST} build ok"
end

task :all => [:osx, :osx_test] do; end

#------------------------------------------------------------------------------
#
# Run
#
#------------------------------------------------------------------------------
task :run => [:osx] do
  sh "./#{APP}"
end
