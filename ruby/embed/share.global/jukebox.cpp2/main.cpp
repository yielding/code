#include "app.h"
#include "ruby19eval.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  Ruby19Eval& vm = *Ruby19Eval::instance();
  App&       app = *App::instance();

  for (int i=10; i<30; i+=10)
  {
    app.set_jukebox(i);
    vm.run_file("embed.rb");
  }

  app.delete_instance();
  vm .delete_instance();

  return 0;
}
