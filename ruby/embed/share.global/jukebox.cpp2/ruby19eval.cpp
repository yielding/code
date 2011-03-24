#include "ruby19eval.h"

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Ruby19Eval* Ruby19Eval::m_instance = 0;

////////////////////////////////////////////////////////////////////////////////
//
// Called by Ruby for writing to STDOUT
//
////////////////////////////////////////////////////////////////////////////////
Ruby19Eval* Ruby19Eval::instance() 
{
  if (!m_instance) 
    m_instance = new Ruby19Eval();

  return m_instance;
}

void Ruby19Eval::delete_instance()
{
  if (m_instance != NULL)
  {
    delete m_instance;
    m_instance = NULL;
  }
}

Ruby19Eval::Ruby19Eval()
{
  ruby_init();
  ruby_init_loadpath();

//#ifndef WIN32
//  ruby_init_loadpath();
//#else
//  string prefix("d:/code/swig");
//  string stdlib  (prefix + "/lib/ruby/1.8");
//  string stdext  (prefix + "/lib/ruby/1.8/i386-msvcr80");
//  string sitebase(prefix + "/lib/ruby/site_ruby");
//  string sitelib (prefix + "/lib/ruby/site_ruby/1.8");
//  string siteext (prefix + "/lib/ruby/site_ruby/1.8/i386-msvcr80");
//
//  ruby_incpush(".");
//  ruby_incpush(sitebase.c_str());
//  ruby_incpush(sitelib.c_str());
//  ruby_incpush(siteext.c_str());
//  ruby_incpush(stdlib.c_str());
//  ruby_incpush(stdext.c_str());
//#endif

  rb_set_safe_level(0);
  ruby_script("ruby");
}

Ruby19Eval::~Ruby19Eval()
{
   // ruby_finalize();
   ruby_cleanup(0);
}

string Ruby19Eval::val2str(const VALUE rval)
{
  VALUE v = rb_funcall(rval, rb_intern("to_s"), 0);
  return StringValueCStr(v);
  // return StringValueCStr(rb_funcall(rval, rb_intern("to_s"), 0));
}

int Ruby19Eval::val2i(const VALUE rval)
{
  return NUM2INT(rval);
}

string Ruby19Eval::strval2str(const VALUE rval)
{
  return string(RSTRING_PTR(rval), RSTRING_LEN(rval));
}

void Ruby19Eval::run_file(const char* filename, ostream& out)
{
  rb_load_protect(rb_str_new2(filename), 0, &m_status);
  if (m_status) 
    exception_print(out);
}

void Ruby19Eval::exception_print(ostream& errout)
{
  // Adapted from eruby_main by Shugo Maeda <shugo@modruby.net>
  if (NIL_P(rb_errinfo())) 
    return;

  VALUE errat = rb_funcall(rb_errinfo(), rb_intern("backtrace"), 0);
  if (!NIL_P(errat)) 
  {
    VALUE mesg = RARRAY_PTR(errat)[0];

    if (NIL_P(mesg)) 
    {
      ID last_func = rb_frame_this_func();
      errout << rb_id2name(last_func);
    } 
    else 
    {
      errout << strval2str(mesg);
    }
  }

  VALUE eclass = CLASS_OF(rb_errinfo());
  VALUE einfo  = rb_obj_as_string(rb_errinfo());
  if (eclass == rb_eRuntimeError && RSTRING_LEN(einfo) == 0) 
  {
    errout << ": unhandled exception\n";
  } 
  else 
  {
    VALUE epath = rb_class_path(eclass);
    if (RSTRING_LEN(einfo) == 0) 
    {
      errout << ": ";
      errout << strval2str(epath);
      errout << "\n";
    } 
    else 
    {
      char *tail  = 0;
      int len = RSTRING_LEN(einfo);

      if (RSTRING_PTR(epath)[0] == '#') epath = 0;
      if ((tail = strchr(RSTRING_PTR(einfo), '\n')) != NULL) 
      {
        len = tail - RSTRING_PTR(einfo);
        tail++;   /* skip newline */
      }
      errout << ": ";
      errout << string(RSTRING_PTR(einfo), len);
      if (epath) 
      {
        errout << " (";
        errout << strval2str(epath);
        errout << ")\n";
      }
      if (tail) 
      {
        errout << string(tail, RSTRING_LEN(einfo) - len - 1);
        errout << "\n";
      }
    }
  }

  if (!NIL_P(errat)) 
  {
#define TRACE_MAX (TRACE_HEAD+TRACE_TAIL+5)
#define TRACE_HEAD 8
#define TRACE_TAIL 5

    rb_ary_pop(errat);
    int ep_len = RARRAY_LEN(errat);
    for (int i=1; i<ep_len; i++) 
    {
      if (TYPE(RARRAY_PTR(errat)[i]) == T_STRING) 
      {
        errout << "        from ";
        errout << strval2str(RARRAY_PTR(errat)[i]);
        errout << "\n";
      }
      if (i == TRACE_HEAD && ep_len > TRACE_MAX) 
      {
        errout << "         ... " << (ep_len - TRACE_HEAD - TRACE_TAIL)
          << " levels...\n";
        i = ep_len - TRACE_TAIL;
      }
    }
  }

  errout.flush();
}     

VALUE Ruby19Eval::eval(const char* code)
{
  return rb_eval_string_protect(code, &m_status);
}

VALUE Ruby19Eval::eval(const char* code, ostream& errout)
{
  VALUE ret = rb_eval_string_protect(code, &m_status);
  if (m_status) 
    exception_print(errout);

  return ret;
}

// Last evaluation was successful
bool Ruby19Eval::evalOk()
{
  return m_status == 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
