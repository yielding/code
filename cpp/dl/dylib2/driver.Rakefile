# -*- ruby -*-

SRC       = "main"
SRCS      = %W{ ./#{SRC}.cpp }
TEST_SRCS = %W{ ./test_#{SRC}.cpp }
APP       = "#{SRC}"
CXX       = "xcrun"
CXXFLAGS  = ":Os"
LIBS      = ""

DIR = File.dirname __FILE__

load '~/code/build/app2.rake'

task :default => [:osx]
