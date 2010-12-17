#-*- ruby -*-   

require 'rake/clean'

task :default => [:osx]

if not defined? APP
  puts "first define APP"
  exit
end

if not defined? SRCS
  puts "define SRCS"
  exit
end

CLEAN.include("*.o")
CLOBBER.include(APP).include("*.exe")

BOOST = {

}

RUBY_PATH    = "/Users/yielding/ruby-mingw32"
MINGW32_DIR  = "/usr/local/i386-mingw32-3.4.5/i386-mingw32"

#OSX_CC       = "g++-mp-4.6 -std=gnu++0x"
OSX_CC       = "g++"
WIN_CC       = "#{MINGW32_DIR}/bin/g++"
WIN_STRIP    = "#{MINGW32_DIR}/bin/strip --strip-all "
LIN_CC       = "g++"

$OSX_LIBS     = ""
$OSX_CXXFLAGS = "-O3 "
$OSX_CXXFLAGS += "-I/opt/local/include/boost"
$OSX_LDFLAGS  = "-L. -L/opt/local/lib"
if defined? OSX_LIBS 
  $OSX_LIBS      = "#{OSX_LIBS}" 
  if OSX_LIBS =~ /ruby19/
    $OSX_CXXFLAGS += " -I/opt/local/include/ruby-1.9.1 -I/opt/local/include/ruby-1.9.1/i386-darwin10.2.0 "
  elsif OSX_LIBS =~ /ruby/
    $OSX_CXXFLAGS += " -I/opt/local/lib/ruby/1.8/i686-darwin10 "
    if OSX_LIBS =~ /rice/
      $OSX_LDFLAGS  += " -L/opt/local/lib/ruby/gems/1.8/gems/rice-1.3.1/ruby/lib/lib"
      $OSX_CXXFLAGS += " -I/opt/local/lib/ruby/gems/1.8/gems/rice-1.3.1/ruby/lib/include" 
    end
  end
end


$WIN_LIBS     = " -lws2_32"
$WIN_CXXFLAGS = "-O2 -D_WIN32_WINNT "
$WIN_LDFLAGS  = "-L. -L#{MINGW32_DIR}/lib "
if defined? WIN_LIBS
  $WIN_LIBS      = "#{WIN_LIBS}" + $WIN_LIBS 
  $WIN_CXXFLAGS += "-I#{RUBY_PATH}/lib/ruby/1.8/i386-mingw32 " if WIN_LIBS =~ /ruby/
  $WIN_LDFLAGS  += "-L#{RUBY_PATH}/lib" if WIN_LIBS =~ /ruby/
end

$LIN_LIBS    = ""
$LIN_LIBS    = "#{LIN_LIBS}" if defined? LIN_LIBS

#------------------------------------------------------------------------------
#
# 
#
#------------------------------------------------------------------------------
class String
  def win32; self + '.win32'; end
  def osx;   self + '.osx';   end
  def linux; self + '.linux'; end
  def o;     self + '.o';     end
  def cpp;   self;            end
  def obj;   self + '.obj';   end
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
    when :win32
      obj.win32.o
    when :linux
      obj.linux.o
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
          sh   "#{OSX_CC} -c #{$OSX_CXXFLAGS} #{obj.cpp} -o #{obj.osx.o}" 
        when :win32
          sh   "#{WIN_CC} -c #{$WIN_CXXFLAGS} #{obj.cpp} -o #{obj.win32.o}" 
        when :linux
          sh   "#{LIN_CC} -c #{$LIN_CXXFLAGS} #{obj.cpp} -o #{obj.linux.o}" 
        end
      end
    end
  end

  def link app, objs 
    objs_os_o = objs.map{|obj| os_o(obj)}.join ' '
    if should_link? app, objs
      case os
      when :osx
        sh   "#{OSX_CC} -o #{app} #{objs_os_o} #{$OSX_LDFLAGS} #{$OSX_LIBS}"
      when :win32                                                         
        sh   "#{WIN_CC} -o #{app} #{objs_os_o} #{$WIN_LDFLAGS} #{$WIN_LIBS}"
        sh   "#{WIN_STRIP} #{app}"
      when :linux
        sh   "#{LIN_CC} -o #{app} #{objs_os_o} #{$LIN_LDFLAGS} #{$LIN_LIBS}"
      end
    end
  end
end

#------------------------------------------------------------------------------
#
# Compile and Link target
#
#------------------------------------------------------------------------------
task :win32_compile do
  builder = Builder.new :os => :win32
  builder.compile SRCS
end

task :win32_link => [:win32_compile] do
  builder = Builder.new :os => :win32
  builder.link APP, SRCS
end

task :osx_compile do
  builder = Builder.new :os => :osx
  builder.compile SRCS
end

task :osx_link => [:osx_compile] do
  builder = Builder.new :os => :osx
  builder.link APP, SRCS
end

task :linux_compile do
  builder = Builder.new :os => :linux
  builder.compile SRCS
end

task :linux_link => [:linux_compile] do
  builder = Builder.new :os => :linux
  builder.link APP, SRCS
end

task :linux => [:linux_link] do; end

#------------------------------------------------------------------------------
#
# Build target
#
#------------------------------------------------------------------------------
task :osx => [:osx_link] do
  puts "#{APP} for osx build ok"
end

task :win32 => [:win32_link] do
  puts "#{APP} for in32 build ok"
  sh   "mv #{APP} #{APP}.exe"
end

task :all => [:win32, :osx] do; end

#------------------------------------------------------------------------------
#
# Run
#
#------------------------------------------------------------------------------
task :run => [:osx] do
  sh "./#{APP}"
end

