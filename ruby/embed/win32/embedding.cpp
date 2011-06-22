#include "stdafx.h"
#include "ruby.h"
#include <iostream>
#include <cstdlib>

using namespace std;

VALUE cTest;
static ID id_push;

static VALUE 
t_init(VALUE self)
{
	VALUE arr = rb_ary_new();
	rb_iv_set(self, "@arr", arr);
	return self;
}

static VALUE 
t_add(VALUE self, VALUE obj)
{
	VALUE arr = rb_iv_get(self, "@arr");
	rb_funcall(arr, id_push, 1, obj);

    return arr;
}

void init_extension()
{
	// 객체를 생성할때 C++ app의 전역 변수를 참조해서 클래스를 만들면 되겠다.
	// 즉, 전역변수를 위한 인터페이스를 따로 만들 필요가 없다.
	cTest = rb_define_class("MyTest", rb_cObject);
	rb_define_method(cTest, "initialize", RUBY_METHOD_FUNC(t_init), 0);
	rb_define_method(cTest, "add", RUBY_METHOD_FUNC(t_add), 1);
    id_push = rb_intern("push");
}

int _tmain(int argc, char* argv[])
{
	ruby_sysinit(&argc, &argv);

	// 1. init interpreter
	RUBY_INIT_STACK;

	ruby_init();

	ruby_script("leech-ruby");

	ruby_init_loadpath();
	ruby_incpush(".");

	init_extension();

	int status;

	// rb_load_protect(rb_str_new2("c:/Users/yielding/test.rb"), 0, &status);
	if (status)
	{
		cout << "error";

		ruby_cleanup(0);
		return EXIT_FAILURE;
	}

	ruby_cleanup(0);

	return EXIT_FAILURE;
}

