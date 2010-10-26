#include <ruby.h>

static VALUE hardware_list;

static void Init_SysInfo()
{
  hardware_list = rb_ary_new();
  rb_global_variable(&hardware_list);
  rb_define_variable("$hardware", &hardware_list);

  rb_ary_push(hardware_list, rb_str_new2("DVD"));
  rb_ary_push(hardware_list, rb_str_new2("CDP1"));
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////////
void print_array(VALUE arr)
{
  int i;
  int size = RARRAY(arr)->len;

  printf("[in C] array elements\n");
  for (i=0; i<size; i++)
  {
    VALUE element = *(RARRAY(arr)->ptr + i);
    char* p = RSTRING(element)->ptr;
    printf("%s ", p);
  }  

  printf("\n");
}

int eval_buffer(char* script)
{
  int state;
  rb_eval_string_protect(script, &state);

  return state;
}

int eval_file(char* path)
{
  int state;
  rb_load_protect(rb_str_new2(path), 0, &state);

  return state;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////////
int main()
{
  // 1. init interpreter
  ruby_init();
  ruby_init_loadpath();
  //ruby_script("embed.rb");

  // 2. load global objects
  Init_SysInfo();

  // 3. run script
  // int state = eval_buffer("puts \"kamin babo\"\nprint $hardware");
  // int state = eval_file("/Users/yielding/test/rb/embed/embed.rb");
  int state = eval_file("embed.rb");
  if (state) 
    printf("error\n");

  print_array(hardware_list);

  // 4. finalize
  // ruby_finalize();
  ruby_cleanup(0);
}
