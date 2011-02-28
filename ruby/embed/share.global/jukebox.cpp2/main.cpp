#include "app.h"
#include "ruby19eval.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  RubyEval& vm = *RubyEval::instance();
  App&     app = *App::instance();

  for (int i=10; i<30; i+=10)
  {
    app.set_jukebox(i);
    vm.run_file("/Users/yielding/code/ruby/embed/share.global/jukebox.cpp2/embed.rb");
  }

  cout << "ok 1\n";
  app.delete_instance();
  cout << "ok 2\n";
  vm .delete_instance();
  cout << "ok 3\n";

  return 0;
}
