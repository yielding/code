#include <ruby.h>
#include <stdio.h>

VALUE hw_mMyModule, hw_cMyClass;

typedef struct greeter_s {
  VALUE name;
} greeter_t;

VALUE 
greet(VALUE self, VALUE names)
{
  int i;
  struct RArray *names_array;
  greeter_t* greeter;
  VALUE my_name;

  Data_Get_Struct(self, greeter_t, greeter);
  my_name = rb_funcall(greeter->name, rb_intern("to_s"), 0);
  names_array = RARRAY(names);

  for (i=0; i<RARRAY_LEN(names_array); i++)
  {
    VALUE current = RARRAY_PTR(names_array)[i];
    if (rb_respond_to(current, rb_intern("to_s")))
    {
      VALUE name = rb_funcall(current, rb_intern("to_s"), 0);
      printf("hello %s, ", StringValuePtr(name));
    }
  }

  printf("I'm %s.\n", StringValuePtr(my_name));

  return Qnil;
}

void greeter_mark(greeter_t* self)
{
  rb_gc_mark(self->name);
}

void greeter_free(greeter_t* self)
{
  free(self);
}

VALUE greeter_allocate(VALUE klass)
{
  greeter_t *g = malloc(sizeof(greeter_t));
  g->name = Qnil;
  return Data_Wrap_Struct(klass, greeter_mark, greeter_free, g);
}

VALUE 
greeter_initialize(VALUE self, VALUE name)
{
  greeter_t* greeter;

  if (!rb_respond_to(name, rb_intern("to_s")))
    rb_raise(rb_eArgError, "name should respond to to_s");

  Data_Get_Struct(self, greeter_t, greeter);
  greeter->name = name;

  return self;
}

void Init_test()
{
  hw_mMyModule = rb_define_module("MyModule");
  hw_cMyClass  = rb_define_class_under(hw_mMyModule, "MyClass", rb_cObject);
  rb_define_alloc_func(hw_cMyClass, greeter_allocate);

  rb_define_method(hw_cMyClass, "greet", greet, 1);
  rb_define_method(hw_cMyClass, "initialize", greeter_initialize, 1);
}
