#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"
#include "rice/Array.hpp"
#include "rice/Enum.hpp"

#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include <map>

using namespace std;
using namespace Rice;

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
namespace
{
  void print(char const* msg)
  {
    cout << "[in c++] " << msg << endl;
  }

  void print(int msg)
  {
    cout << "[in c++] " << msg << endl;
  }

  void update_screen()
  {
    print("screen updated");
  }
}

namespace native
{
  int const  VIM_MAJOR = 7;
  int const  VIM_MINOR = 2;

  class window
  {
  public:
    window()
      : m_width(80), m_height(100), m_xpos(20), m_ypos(100)
    {}

    virtual ~window()
    {}

    void xpos(int x)   { m_xpos = x;      }
    int  xpos()        { return m_xpos;   }
    void ypos(int y)   { m_ypos = y;      }
    int  ypos()        { return m_ypos;   }

    void height(int h) { m_height = h;    }
    int  height()      { return m_height; }

    void width(int w)  { m_width  = w;    }
    int  width()       { return m_width;  }

  private:
    int m_width, m_height;
    int m_xpos, m_ypos;
  };

  class buffer
  {
  public:
    buffer()
    {}

    virtual ~buffer ()
    {}

  private:
    /* data */
  };

}

namespace native
{
  int g_current_window;
  std::vector<native::window> g_windows;
  
  int g_current_buffer;
  std::vector<native::buffer> g_buffers;
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
namespace vim // exception
{
  class VimException: public std::exception
  {
  public:
    VimException(string msg) :m_msg(msg) {}
    virtual ~VimException() throw() {}
    string m_msg;
  };
  
  void throw_test(Object o)
  {
    throw VimException("raised by throw_test");
  }
  
  void handle_vim_exception(VimException const& ex)
  {
    throw Exception(rb_eRuntimeError, "%s", ex.m_msg.c_str());
  }
}

namespace vim // module function
{
  // public functions
  string description(Object o)
  {
    return "leech";
  }
  
  string set_option(Object o, Object v)
  {
    string const& value = from_ruby<std::string>(v);
    if (value != "set exception")
    {
      return string("called ") + value;
    }
    
    // moudle_singleton_function을 사용하지 않는 더 자연스런 방법이 되겠다.
    throw Exception(rb_eRuntimeError, "xxxx");
  }
}

namespace vim // wrapping class
{
  using namespace native;
  
  class WindowExt
  {
  public:
    WindowExt(native::window* w=NULL)
      : m_window(w)
    {
      m_window->height(80);
      m_window->width(40);
      m_window->xpos(10);
      m_window->ypos(20);
    }
    
    virtual ~WindowExt()
    {
    }
    
    int  height()      { return m_window->height(); }
    void height(int h) { m_window->height(h);       }
    
    int  width()       { return m_window->width();  }
    void width(int w)  { m_window->width(w);        }

    void set_cursor(Object o)
    {
      Rice::Array a = to_ruby<Rice::Array>(o);
      if (a.size() != 2)
        throw Exception(rb_eRuntimeError, "argc should be 2");
        
      m_window->xpos(from_ruby<int>(a[0]));
      m_window->ypos(from_ruby<int>(a[1]));
    }

    Object get_cursor()
    {
      Array arr;
      arr.push(to_ruby(m_window->xpos()));
      arr.push(to_ruby(m_window->ypos()));
      
      return to_ruby<Object>(arr);
    }
    
  private:
    native::window* m_window;
    
  public:
    static int count(Object self)
    {
      return g_windows.size();
    }
    
    static Object current(Object self)
    {
      WindowExt* we = new WindowExt(&g_windows[g_current_window]);
      return Data_Object<WindowExt>(we);
    }
  
    static Object bracket(Object self, int index)
    {
      if (g_windows.size() < index)
        throw Exception(rb_eRuntimeError, "index out of range");

      return Data_Object<WindowExt>(
        new WindowExt(&g_windows[g_current_window]));
    }
  };
  
  typedef int  (WindowExt::*getter)();
  typedef void (WindowExt::*setter)(int);

  class BufferExt
  {
  public:
    BufferExt(native::buffer* buf=0)
      : m_buf(buf)
    {
    }
    
    virtual ~BufferExt()
    {
      // REMARK
      // 고의적으로 m_buf를 지우지 않는다. GC에게 맡겼다.
    }
    
  private:
    native::buffer* m_buf;
    
  public:
    static int count(Object self)
    {
      return g_buffers.size();
    }
    
    static Object current(Object self)
    {
      BufferExt* be = new BufferExt(&g_buffers[0]);
      return Data_Object<BufferExt>(be);
    }
  
    static Object bracket(Object self, int index)
    {
      if (g_buffers.size() < index)
        throw Exception(rb_eRuntimeError, "index out of range");

      return Data_Object<BufferExt>(new BufferExt(&g_buffers[index]));
    }
  };
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
void Init_Extension()
{
  using namespace vim;
  
  RUBY_TRY
  {
    Rice::Module rb_mVIM 
      = define_module("VIM")
        .add_handler<VimException>(handle_vim_exception)
        .define_singleton_method("throw_test", throw_test)
        .const_set(rb_intern("VIM_MAJOR"), to_ruby(native::VIM_MAJOR))
        .const_set(rb_intern("VIM_MINOR"), to_ruby(native::VIM_MINOR))
        .define_module_function("desc", description)
        .define_module_function("set_option", set_option)
        ;
  
    Rice::Data_Type<WindowExt> rb_cWindow
      = define_class_under<WindowExt>(rb_mVIM, "Window")
        .define_constructor(Constructor<WindowExt>())
        .define_singleton_method("count", &WindowExt::count)
        .define_singleton_method("[]", &WindowExt::bracket)
        .define_singleton_method("current", &WindowExt::current)
        .define_method("height",  getter(&WindowExt::height))
        .define_method("height=", setter(&WindowExt::height))
        .define_method("width",   getter(&WindowExt::width))
        .define_method("width=",  setter(&WindowExt::width))
        .define_method("cursor",  &WindowExt::get_cursor)
        .define_method("cursor=", &WindowExt::set_cursor)
        ;
        
    Rice::Data_Type<BufferExt> rb_cBuffer
      = define_class_under<BufferExt>(rb_mVIM, "Buffer")
        .define_constructor(Constructor<BufferExt>())
        .define_singleton_method("count", &BufferExt::count)
        .define_singleton_method("[]", &BufferExt::bracket)
        .define_singleton_method("current", &BufferExt::current)
        ;
  }
  RUBY_CATCH

  for (int i=0; i<5; i++)
  {
    g_windows.push_back(native::window());
    g_current_window = i;
  }

  for (int i=0; i<10; i++)
  {
    g_buffers.push_back(native::buffer());
    g_current_buffer = i;
  }
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
int eval_file(char const* path)
{
  int state;
  rb_load_protect(rb_str_new2(path), 0, &state);

  return state;
}

int main(int argc, char** argv)
{
  // 0.
  ruby_sysinit(&argc, &argv);

  // 1. init interpreter
  RUBY_INIT_STACK;

  VALUE v;
  Init_stack(&v);

  ruby_init();

  ruby_script("leech-ruby");

  ruby_init_loadpath();

  // rb_enc_find_index("encdb");

  // 2. register c-based extension module
  Init_Extension();

  // 3. register global instance

  // 4. run script
  int state = eval_file("/Users/yielding/code/ruby/embed/rice/if_vim2/embed.rb");
  if (state) 
  {
    printf("error on eval_file(\"embed.rb\");\n");
  }

  // 5. finalize
  ruby_cleanup(0);
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
