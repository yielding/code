#include <stdio.h>

#include <ruby.h>

int main(int argc, char *argv[]) 
{
  ruby_init();
  ruby_init_loadpath();
  RUBY_INIT_STACK;

  rb_include_module(rb_cObject, rb_mKernel);
  rb_include_module(rb_cObject, rb_mComparable);
  rb_include_module(rb_cObject, rb_mEnumerable);
  rb_include_module(rb_cObject, rb_mPrecision);
  rb_include_module(rb_cObject, rb_mErrno);
  rb_include_module(rb_cObject, rb_mFileTest);
  rb_include_module(rb_cObject, rb_mGC);
  rb_include_module(rb_cObject, rb_mMath);
  rb_include_module(rb_cObject, rb_mProcess);

  rb_eval_string("print \"Hello world\\n\"");

  int status = ruby_exec();
  int res = 0;
  if (status) 
  {
    printf("%s\n", StringValueCStr(ruby_errinfo));
    res = 1;
  }
  ruby_finalize();

  return res;
}
