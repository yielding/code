# -*- ruby -*-

SRC       = "triangle"
SRCS      = %W{ ./#{SRC}.cpp }
TEST_SRCS = %W{ ./test_#{SRC}.cpp }
APP       = "#{SRC}.dylib"
CXX       = "xcrun"
CXXFLAGS  = ":Os"
LIBS      = ""

DIR = File.dirname __FILE__

task :default => [:osx_dylib_link]

load '~/code/build/app2.rake'
