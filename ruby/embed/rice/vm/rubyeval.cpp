#include "rubyeval.h"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
RubyEval* RubyEval::m_instance = 0;

namespace
{
}

////////////////////////////////////////////////////////////////////////////////
//
// Called by Ruby for writing to STDOUT
//
////////////////////////////////////////////////////////////////////////////////
RubyEval* RubyEval::instance() 
{
  if (!m_instance) 
    m_instance = new RubyEval();

  return m_instance;
}

void RubyEval::delete_instance()
{
  if (m_instance)
  {
    delete m_instance;
    m_instance = NULL;
  }
}

RubyEval::RubyEval()
{
  ruby_init();

#ifndef WIN32
  ruby_init_loadpath();
#else
  string prefix("d:/code/swig");
  string stdlib  (prefix + "/lib/ruby/1.8");
  string stdext  (prefix + "/lib/ruby/1.8/i386-msvcr80");
  string sitebase(prefix + "/lib/ruby/site_ruby");
  string sitelib (prefix + "/lib/ruby/site_ruby/1.8");
  string siteext (prefix + "/lib/ruby/site_ruby/1.8/i386-msvcr80");

  ruby_incpush(".");
  ruby_incpush(sitebase.c_str());
  ruby_incpush(sitelib.c_str());
  ruby_incpush(siteext.c_str());
  ruby_incpush(stdlib.c_str());
  ruby_incpush(stdext.c_str());
#endif

  rb_set_safe_level(0);
  ruby_script("ruby");
}

RubyEval::~RubyEval()
{
  ruby_finalize();
}

string RubyEval::val2str(const VALUE rval)
{
  return StringValueCStr(rb_funcall(rval, rb_intern("to_s"), 0));
}

int RubyEval::val2i(const VALUE rval)
{
  return NUM2INT(rval);
}

string RubyEval::strval2str(const VALUE rval)
{
  return string(RSTRING(rval)->ptr, RSTRING(rval)->len);
}

void RubyEval::run_file(const char* filename, ostream& out)
{
  rb_load_protect(rb_str_new2(filename), 0, &m_status);
  if (m_status) 
    exception_print(out);
}

void RubyEval::exception_print(ostream& errout)
{
  // Adapted from eruby_main by Shugo Maeda <shugo@modruby.net>
  if (NIL_P(ruby_errinfo)) 
    return;

  VALUE errat = rb_funcall(ruby_errinfo, rb_intern("backtrace"), 0);
  if (!NIL_P(errat)) 
  {
    VALUE mesg = RARRAY(errat)->ptr[0];

    if (NIL_P(mesg)) 
    {
      ID last_func = rb_frame_last_func();
      errout << rb_id2name(last_func);
    } 
    else 
    {
      errout << strval2str(mesg);
    }
  }

  VALUE eclass = CLASS_OF(ruby_errinfo);
  VALUE einfo  = rb_obj_as_string(ruby_errinfo);
  if (eclass == rb_eRuntimeError && RSTRING(einfo)->len == 0) 
  {
    errout << ": unhandled exception\n";
  } 
  else 
  {
    VALUE epath = rb_class_path(eclass);
    if (RSTRING(einfo)->len == 0) 
    {
      errout << ": ";
      errout << strval2str(epath);
      errout << "\n";
    } 
    else 
    {
      char *tail  = 0;
      int len = RSTRING(einfo)->len;

      if (RSTRING(epath)->ptr[0] == '#') epath = 0;
      if ((tail = strchr(RSTRING(einfo)->ptr, '\n')) != NULL) 
      {
        len = tail - RSTRING(einfo)->ptr;
        tail++;   /* skip newline */
      }
      errout << ": ";
      errout << string(RSTRING(einfo)->ptr, len);
      if (epath) 
      {
        errout << " (";
        errout << strval2str(epath);
        errout << ")\n";
      }
      if (tail) 
      {
        errout << string(tail, RSTRING(einfo)->len - len - 1);
        errout << "\n";
      }
    }
  }

  if (!NIL_P(errat)) 
  {
    struct RArray *ep = RARRAY(errat);

#define TRACE_MAX (TRACE_HEAD+TRACE_TAIL+5)
#define TRACE_HEAD 8
#define TRACE_TAIL 5

    rb_ary_pop(errat);
    ep = RARRAY(errat);
    for (int i=1; i<ep->len; i++) 
    {
      if (TYPE(ep->ptr[i]) == T_STRING) 
      {
        errout << "        from ";
        errout << strval2str(ep->ptr[i]);
        errout << "\n";
      }
      if (i == TRACE_HEAD && ep->len > TRACE_MAX) 
      {
        errout << "         ... " << (ep->len - TRACE_HEAD - TRACE_TAIL)
          << " levels...\n";
        i = ep->len - TRACE_TAIL;
      }
    }
  }

  errout.flush();
}     

VALUE RubyEval::eval(const char* code)
{
  return rb_eval_string_protect(code, &m_status);
}

VALUE RubyEval::eval(const char* code, ostream& errout)
{
  VALUE ret = rb_eval_string_protect(code, &m_status);
  if (m_status) 
    exception_print(errout);

  return ret;
}

// Last evaluation was successful
bool RubyEval::evalOk()
{
  return m_status == 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
