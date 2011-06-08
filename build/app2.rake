#-*- ruby -*-   

require 'rake/clean'

if not defined? APP
  puts "define APP"
  exit
end

if not defined? SRCS
  puts "define sources first"
  exit
end

$CXX = "g++"
if defined? CXX
  if defined? APP_TEST
    $CXX = "clang++"
  else
    $CXX = "clang++"    if CXX =~ /clang\+\+/
    $CXX = "g++-mp-4.6 -std=gnu++0x" if CXX =~ /c\+\+0x/
  end
end

$CXXFLAGS =" -DPOSIX -std=c++0x"
if defined? CXXFLAGS
  CXXFLAGS.split.each do |f|
    flag = case f
           when /:w/ ; " -Wall"
           when /:v/ ; " -DVERBOSE"
           when /:d/ ; " -g -DDEBUG"
           when /:O0/; " -O0"
           when /:O3/; " -O3"
           when /:Os/; " -Os"
           else
             " -D#{f}"
           end

    $CXXFLAGS += flag
  end
end

$INCS = " -I. -I/opt/local/include"
INCS.split.each { |e| $INCS += " -I#{e}" } if defined? INCS

$LDFLAGS = " -L. -L/opt/local/lib"
if defined? LDFLAGS
  LDFLAGS.split.each do |e|
    flag = case e
           when /:framework/; " -F/System/Library/PrivateFrameworks"
           when /:dylib/; " -dynamiclib -arch x86_64 -Wl,-syslibroot,/Developer/SDKs/MacOSX10.6.sdk"
           else
             " -L#{e}"
           end

    $LDFLAGS += flag
  end
end

BOOST = {
  :t => " -lboost_thread-mt",     :s => " -lboost_system-mt",
  :f => " -lboost_filesystem-mt", :d => " -lboost_date_time-mt",
  :r => " -lboost_regex-mt"
}

$LIBS = ""
if defined? LIBS
  LIBS.split.each do |e|  
    $LIBS += case e
             when /:t/; BOOST[:t]
             when /:s/; BOOST[:s]
             when /:f/; BOOST[:f]
             when /:d/; BOOST[:d]
             when /:r/; BOOST[:r]
             else
               " -l#{e}"
             end
  end
end

$FRAMEWORKS = ""
if defined? FRAMEWORKS
  arr = FRAMEWORKS.split
  arr.each { |e| $FRAMEWORKS += " -framework #{e}" }
end

$OUTDIR = "."
$OUTDIR = OUTDIR if defined? OUTDIR

def out_path_of file
  filename = file.split("/")[-1]
  "#{$OUTDIR}/#{filename}"
end

OBJS = SRCS.map { |src| out_path_of "#{src}.osx.o" }

CLEAN  .include(OBJS)
CLOBBER.include(APP)

if defined? APP_TEST
  TEST_OBJS = TEST_SRCS.map { |src| out_path_of "#{src}.osx.o" }
  CLEAN  .include(TEST_OBJS)
  CLOBBER.include(APP_TEST)
end

#------------------------------------------------------------------------------
#
# 
#
#------------------------------------------------------------------------------
class String
  def osx; self + '.osx'; end
  def win; self + '.win'; end
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

  def should_compile? src
    src = File.expand_path(src)
    obj = out_path_of os_o(src)
    if File.exist? obj
      src_time = File.mtime src.cpp
      obj_time = File.mtime obj
      src_time > obj_time
    else
      true
    end
  end

  def should_link? bin, objs 
    if File.exist? bin 
      mtime = File.mtime bin 
      objs.any? { |obj| File.mtime(out_path_of os_o(obj)) > mtime }
    else
      true
    end
  end

  def compile objs=[] 
    objs.each do |obj|
      if should_compile? obj
        case os 
        when :osx
          path = out_path_of obj.osx.o
          `#{$CXX} -c #{$CXXFLAGS} #{$INCS} #{obj.cpp} -o #{path}`
        end
      end
    end
  end

  def link app, objs
    objs_os_o = objs.map { |obj| out_path_of os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        `#{$CXX} -o #{app} #{objs_os_o} #{$LDFLAGS} #{$FRAMEWORKS} #{$LIBS}`
      end
    end
  end

  def link_test app, objs
    objs_os_o = objs.map { |obj| os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        `#{$CXX} -o #{app} #{objs_os_o} #{$LDFLAGS} #{$FRAMEWORKS} #{$LIBS} -lgtest -lgtest_main`
      end
    end
  end

  def link_lib app, objs
    objs_os_o = objs.map { |obj| os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        `(ar crl #{app} #{objs_os_o}) && ranlib #{app}`
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

task :osx_lib_link => [:osx_compile] do
  builder = Builder.new :os => :osx
  builder.link_lib APP, SRCS
end

task :osx_test_link => [:osx_test_compile] do
  builder = Builder.new :os => :osx
  builder.link_test APP_TEST, TEST_SRCS
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

task :dylib => [:osx_link] do
  puts "#{APP} build ok"
end

task :lib => [:osx_lib_link] do
  puts "#{APP} build ok"
end

task :osx_test => [:osx_test_link] do
  puts "#{APP_TEST} build ok"
end

task :test => [:osx_test_link] do
  puts "#{APP_TEST} build ok"
end

# overridable
# task :all => [:osx, :osx_test] do; end

#------------------------------------------------------------------------------
#
# Run
#
#------------------------------------------------------------------------------
task :run => [:osx] do
  sh "./#{APP}"
end

task :run_test => [:osx_test] do
  sh "./#{APP_TEST}"
end
