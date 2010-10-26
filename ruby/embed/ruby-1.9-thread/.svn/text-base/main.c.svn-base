#include <ruby.h>
#include <stdio.h>

int init_ruby_vm_1(int argc, char* argv[])
{
  ruby_sysinit(&argc, &argv);
  ruby_init();
  ruby_init_loadpath();

  char* rbargv[2];
  rbargv[0] = 0;
  rbargv[1] = argv[1];

  return  ruby_run_node(ruby_options(2, rbargv));
}

// 한번만 실행됨
int run_script1(char* filename)
{
  char* rbargv[2];
  rbargv[0] = 0;
  rbargv[1] = filename;

  return ruby_run_node(ruby_options(2, rbargv));
}

int run_script2(char const* filename)
{
  int status = 0;
  rb_load_protect(rb_str_new2(filename), 0, &status);

  return status == 0;
}

int init_ruby_vm(int argc, char* argv[])
{
  ruby_sysinit(&argc, &argv);
  ruby_init();
  ruby_init_loadpath();
}

int main(int argc, char* argv[])
{
  int i;
  if (argc != 2)
  {
    printf("usage main.so hello.rb\n");
    return 0;
  }

  init_ruby_vm(argc, argv);
  for (i=0; i<20; i++)
  {
    run_script2(argv[1]);
    sleep(1);
  }

  return ruby_cleanup(0);
}
