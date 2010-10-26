// #include <iostream>
#include "rubyeval.h"

extern "C" void Init_DataModel();

int main (int argc, char const* argv[])
{
  RubyEval& ruby = *RubyEval::instance();
  Init_DataModel();
  ruby.run_file("test1.rb");

  return 0;
}
