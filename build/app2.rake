#-*- ruby -*-   
require 'rake/clean'

include Rake::DSL

if not defined? APP
  puts "define APP"
  exit
end

if not defined? SRCS
  puts "define sources first"
  exit
end

$CXX = "ccache g++-15 -std=c++23"
if defined? CXX
  $CXX = "ccache clang++ -std=c++2c -stdlib=libc++ " if CXX == "clang++"
  $CXX = "ccache g++-15 -std=c++23 " if CXX == "g++"
  $CXX = "ccache xcrun clang++ -std=c++23 -stdlib=libc++ " if CXX == "xcrun" 
end

$HOME = ENV["HOME"]

$CXXFLAGS = " -DPOSIX"
if defined? CXXFLAGS
  CXXFLAGS.split.each do |f|
    flag = case f
           when /:w/ ; " -Wall"
           when /:v/ ; " -DVERBOSE"
           when /:d/ ; " -g -DDEBUG"
           when /:O0/; " -O0"
           when /:O3/; " -O3"
           when /:Os/; " -Os"
           when /:arc/; " -fobjc-arc"
           when /:objc/; " -ObjC++"
           when /:enum/; " -Wno-deprecated-enum-enum-conversion"
           else
             " -D#{f}"
           end

    $CXXFLAGS += flag
  end
end

# YRV : YARV 
MVM     = "#{$HOME}/opensource/mruby"
MVM_INC = "#{MVM}/include"

$INCS = " -I#{ENV['CPLUS_INCLUDE_PATH']}"
if defined? INCS
  INCS.split.each do |i|
     flag = case i
            when /:mvm/ ; " -I#{MVM_INC}"
            when /:py/  ; " -I/usr/local/Frameworks/Python.framework/Headers"
            else
             " -I#{i}"
            end
     $INCS = flag + $INCS
  end
end

$INCS += " -I. -I/usr/local/include -I#{$HOME}/develop/include -I/opt/homebrew/include"
$INCS += " -I#{$HOME}/develop/vendor/include"

$LDFLAGS = " #{ENV['LDFLAGS']}"
if defined? LDFLAGS
  LDFLAGS.split.each do |e|
    sdk_path = "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
    flag = case e
           when /:framework/; " -F/System/Library/PrivateFrameworks"
           when /:dylib/    ; " -dynamiclib -arch x86_64 -Wl,-syslibroot,-undefined #{sdk_path}"
           when /:pytorch/  ; " -Wl,-undefined,dynamic_lookup,-rpath,#{PYTORCH_LIB}/lib -L#{PYTORCH_LIB}/lib"
           when /:mvm/      ; " -L#{MVM}/build/host/lib -lmruby"
           when /:smvm/     ; " -L#{MVM}/build/host/lib -l:mruby.dylib"
           else
             " -L#{e}"
           end

    $LDFLAGS += flag
  end
end

$LDFLAGS += " -L. -L/usr/local/lib -L#{$HOME}/develop/vendor/lib"

BOOST = {
  :c => " -lboost_chrono",
  :d => " -lboost_date_time",
  :f => " -lboost_filesystem",
  :m => " -lboost_timer",
  :o => " -lboost_program_options",
  :r => " -lboost_regex",
  :s => " -lboost_system",
  :t => " -lboost_thread"
}

$LIBS = ""
if defined? LIBS
  LIBS.split.each do |e|  
    $LIBS += case e
             when /:c/; BOOST[:c]
             when /:d/; BOOST[:d]
             when /:f/; BOOST[:f]
             when /:m/; BOOST[:m]
             when /:o/; BOOST[:o]
             when /:r/; BOOST[:r]
             when /:s/; BOOST[:s]
             when /:t/; BOOST[:t]
             else
               " -l#{e}"
             end
  end
end

$SLIBS = ""
if defined? SLIBS
  SLIBS.split.each { |e| $SLIBS += " #{e}" }
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
    obj = out_path_of(os_o(src))
    if File.exist? obj
      src_time = File.mtime(src.cpp)
      obj_time = File.mtime(obj)
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
          sh "#{$CXX} -c #{$CXXFLAGS} #{$INCS} #{obj.cpp} -o #{path}"
        end
      end
    end
  end

  def link app, objs
    objs_os_o = objs.map { |obj| out_path_of os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        sh "#{$CXX} -o #{app} #{objs_os_o} #{$LDFLAGS} #{$FRAMEWORKS} #{$LIBS} #{$SLIBS}"
      end
    end
  end

  def link_test app, objs
    objs_os_o = objs.map { |obj| out_path_of os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        sh "#{$CXX} -o #{app} #{objs_os_o} #{$LDFLAGS} #{$FRAMEWORKS} #{$LIBS} -lgtest -lgmock -lgtest_main"
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

  def link_dylib app, objs
    objs_os_o = objs.map { |obj| os_o(obj) }.join ' '
    if should_link? app, objs
      case os
      when :osx
        sh "#{$CXX} -dynamiclib -arch x86_64 -o #{app} #{objs_os_o} #{$LDFLAGS}  #{$LIBS}"
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

task :osx_dylib_link => [:osx_compile] do
  builder = Builder.new :os => :osx
  builder.link_dylib APP, SRCS
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
#task :default => [:osx]

task :osx => [:osx_link] do
  puts "#{APP} build ok"
end

task :dylib => [:osx_dylib_link] do
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
