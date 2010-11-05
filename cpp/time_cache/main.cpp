#include "rubyeval.h"
#include "data_model_wrapper.h"

int main (int argc, char const* argv[])
{
  RubyEval& ruby = *RubyEval::instance();
  Init_DataModel();
  ruby.run_file("test1.rb");

  return 0;
}
