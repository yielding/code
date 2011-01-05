#include "rubyeval.h"

#include <cassert>

int main(int argc, char *argv[])
{
  RubyEval& ruby = *RubyEval::instance();

  ruby.eval("puts 'hello ruby'");
  ruby.run_file("hi.rb");

  assert(RubyEval::val2i(ruby.eval("1+1")) == 2);

  assert(RubyEval::val2str(ruby.eval("'Regexp'.gsub(/x/, 'X')")) == "RegeXp");

  return 0;
}
