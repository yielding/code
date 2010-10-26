#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"
#include "rice/Exception.hpp"
#include "rice/Array.hpp"
#include "rice/Enum.hpp"

#include <iostream>
#include <string>
#include <vector>
#include <map>

using namespace std;
using namespace Rice;

//////////////////////////////////////////////////////////////////////////////
//
// 아래 클래스는 Ruby에서 STL을 사용할 수 있도록 하는데 의미있는것.
// 실제로 이 클래스를 쓸 필요는 없다.
//
//////////////////////////////////////////////////////////////////////////////
class Map
{
public:
  struct Ruby_Value_Compare
  {
    bool operator()(Object lhs, Object rhs) const
    {
      Object result = lhs.call("<", rhs);
      return result.test();
    }
  };
  
  typedef std::map<Object, Object, Ruby_Value_Compare> Value_Map;
  typedef Value_Map::value_type value_type;
  typedef Value_Map::iterator iterator;

  Value_Map::iterator begin() { return map_.begin(); }
  Value_Map::iterator end()   { return map_.end();   }
  
  Object bracket_equals(Object k, Object v)
  {
    map_[k] = v;
    return Qnil;
  }
  
  Object bracket(Object k)
  {
    Value_Map::iterator it = map_.find(k);
    return it == map_.end() ? Object(Qnil) : it->second;
  }
  
private:
  Value_Map map_;
};

template<>
Object to_ruby<Map::value_type>(Map::value_type const& p)
{
  return protect(rb_assoc_new, p.first, p.second);
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
/*
class CDJukebox
{
public:
  CDJukebox(int unit): m_unit_id(unit)
  {
    cout << "CDJukebox ctor with: " << unit << endl;
  }

  CDJukebox(CDJukebox const& rhs)
  {
    if (this != &rhs)
      m_unit_id = rhs.m_unit_id;
  }

  ~CDJukebox() 
  {
    cout << "CDJukebox dtor with: " << m_unit_id << endl;
  }

  void assign(int unit_id) { m_unit_id = unit_id; }

  void seek(int disc, int track)
  {
    cout << "disc: " << disc << " track: " << track << endl;
  }

  int unit() { return m_unit_id; }

  int set_unit(int unit) { return m_unit_id = unit; }

  double avg_seek_time() { return 1.23; }

  void set_strings(Object o)
  {
    Rice::Array a = to_ruby<Rice::Array>(o);
    Rice::Array::iterator it = a.begin();
    Rice::Array::iterator end = a.end();
    cout << "from ruby to c++ \n";
    while (it != end)
    {
      string s = from_ruby<string>(*it);
      m_strs.push_back(s);
      cout << s << " ";
      ++it;
    }
  }

  Object get_strings()
  {
    Array arr;
    for (size_t i=0; i<m_strs.size(); ++i) 
      arr.push(to_ruby(m_strs[i]));
      
    return to_ruby<Object>(arr);
  }

  void set_jukeboxes(Object o)
  {
    Rice::Array a = to_ruby<Rice::Array>(o);
    Rice::Array::iterator it = a.begin();
    Rice::Array::iterator end = a.end();
    while (it != end)
    {
      Data_Object<CDJukebox> obj(*it);
      cout << obj.get()->unit();
      ++it;
    }
  }

  Object get_jukeboxes()
  {
    Data_Object<CDJukebox> o1(new CDJukebox(1000));
    Data_Object<CDJukebox> o2(new CDJukebox(2000));
    Array arr;
    arr.push(o1);
    arr.push(o2);

    return to_ruby<Object>(arr);
  }

private:
  int m_unit_id;
  vector<string> m_strs;
};
*/

//////////////////////////////////////////////////////////////////////////////
//
// 아래 변수가 실제 전역일 필요는 없지 app singleton에 있어도 충분해 보인다.
//
//////////////////////////////////////////////////////////////////////////////
//void register_global()
//{
//  g_box->set_unit(10);
//
//  Data_Object<CDJukebox> obj(g_box);
//  g_jukebox = obj.value();
//  rb_global_variable(&g_jukebox);
//  rb_define_variable("$jukebox", &g_jukebox);
//}

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

namespace vim
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

namespace vim
{
  int const  VIM_MAJOR = 7;
  int const  VIM_MINOR = 2;
  
  // public functions
  string description(Object o)
  {
    return "leech";
  }
  
  void set_option(Object o, Object v)
  {
    string const& value = from_ruby<std::string>(v);
    print(value.c_str());
    if (value != "set exception")
    {
      update_screen();
      return;
    }
    
    // moudle_singleton_function을 사용하지 않는 더 자연스런 방법이 되겠다.
    throw Exception(rb_eRuntimeError, "xxxx");
  }
  
}

namespace vim
{
  class Window;
  int g_current_window;
  std::vector<Window> g_windows;
  
  class Buffer;
  int g_current_buffer;
  std::vector<Buffer> g_buffers;
}

namespace vim
{
  class Window
  {
  public:
    Window()
      : m_height(80)
      , m_width(40)
    {
    }
    
    virtual ~Window()
    {
    }
    
    int  height()      { return m_height; }
    void height(int h) { m_height = h;    }
    
    int  width()       { return m_width;  }
    void width(int w)  { m_width = w;     }
    
    
    static int count(Object self)
    {
      return g_windows.size();
    }
    
    static Object current(Object self)
    {
      return Data_Object<Window>(&g_windows[g_current_window],
               Data_Type<vim::Window>::klass(), 
               0, 0);  // disable gc 
    }
  
    static Object bracket(Object self, int index)
    {
      if (g_windows.size() < index)
        throw Exception(rb_eRuntimeError, "index out of range");

      return Data_Object<vim::Window>(&g_windows[index], 
               Data_Type<vim::Window>::klass(),
               0, 0);  // disable GC
    }
    
  private:
    int m_height;
    int m_width;
  };
  
  class Buffer
  {
  public:
    Buffer()
    {}
    
    virtual ~Buffer()
    {}
    
    
    static int count(Object self)
    {
      return g_buffers.size();
    }
    
    static Object current(Object self)
    {
      return Data_Object<Buffer>(&g_buffers[g_current_buffer],
               Data_Type<vim::Buffer>::klass(), 
               0, 0);  // disable gc 
    }
  
    static Object bracket(Object self, int index)
    {
      if (g_buffers.size() < index)
        throw Exception(rb_eRuntimeError, "index out of range");

      return Data_Object<Buffer>(&g_buffers[index],
               Data_Type<vim::Buffer>::klass(), 
               0, 0);  // disable gc 
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
        .const_set(rb_intern("VIM_MAJOR"), to_ruby(VIM_MAJOR))
        .const_set(rb_intern("VIM_MINOR"), to_ruby(VIM_MINOR))
        .define_module_function("desc", description)
        .define_module_function("set_option", set_option)
        ;

    Rice::Data_Type<Window> rb_cWindow
      = define_class_under<Window>(rb_mVIM, "Window")
          .define_constructor(Constructor<Window>())
          .define_singleton_method("count", &Window::count)
          .define_singleton_method("[]", &Window::bracket)
          .define_singleton_method("current", &Window::current)
        ;
        
    Rice::Data_Type<Buffer> rb_cBuffer
      = define_class_under<Buffer>(rb_mVIM, "Buffer")
          .define_constructor(Constructor<Buffer>())
          .define_singleton_method("count", &Buffer::count)
          .define_singleton_method("[]", &Buffer::bracket)
          .define_singleton_method("current", &Buffer::current)
        ;
        
    Map::iterator (Map::*begin)() = &Map::begin;
    Map::iterator (Map::*end)() = &Map::end;

    Rice::Module rb_mStd = define_module("Std");
    Data_Type<Map> rb_cMap =
      define_class_under<Map>(rb_mStd, "Map")
      .define_constructor(Constructor<Map>())
      .define_method("[]", &Map::bracket)
      .define_method("[]=", &Map::bracket_equals)
      .define_iterator(begin, end)
      .include_module(rb_mEnumerable)
      ;
      
  }
  RUBY_CATCH


  for (int i=0; i<5; i++)
  {
    g_windows.push_back(Window());
    g_current_window = i;
  }

  for (int i=0; i<10; i++)
  {
    g_buffers.push_back(Buffer());
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

int main()
{
  // 1. init interpreter
  ruby_init();
  ruby_init_loadpath();

  // 2. register c-based extension module
  Init_Extension();

  // 3. register global instance

  // 4. run script
  int state = eval_file("embed.rb");
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
