#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"

#include <iostream>

using namespace std;
using namespace Rice;

class CDJukebox
{
public:
  CDJukebox(int unit)
  {
    cout << "CDJukebox ctor\n";
    m_unit_id = unit;
  }

  CDJukebox(CDJukebox const& rhs)
  {
    if (this != &rhs)
    {
      cout << "copy constructor is called\n";

      m_unit_id = rhs.m_unit_id;
    }
  }

  ~CDJukebox()
  {
    cout << "CDJukebox dtor\n";
  }

  void assign(int unit_id)
  {
    m_unit_id = unit_id;
  }

  void seek(int disc, int track)
  {
    cout << "disc: " << disc << " track: " << track << endl;
  }

  int unit()
  {
    return m_unit_id;
  }

  int set_unit(int unit)
  {
    return m_unit_id = unit;
  }

  double avg_seek_time()
  {
    return 1.23;
  }

private:
  int   m_statusf;
  int   m_request;
  void* m_data;
  char  m_pending;
  int   m_unit_id;
  void* m_stats;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int eval_file(char const* path)
{
  int state;
  rb_load_protect(rb_str_new2(path), 0, &state);

  return state;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
VALUE g_jukebox;
CDJukebox *g_box = new CDJukebox(123);

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void register_global()
{
  Data_Object<CDJukebox> obj(g_box);
  g_jukebox = obj.value();
  rb_global_variable(&g_jukebox);
  rb_define_variable("$jukebox", &g_jukebox);
}

//extern "C" 
void Init_CDJukebox()
{
  define_class<CDJukebox>("CDJukebox")
    .define_constructor(Constructor<CDJukebox, int>())
    .define_method("seek", &CDJukebox::seek)
    .define_method("seek_time", &CDJukebox::avg_seek_time)
    .define_method("unit", &CDJukebox::unit)
    ;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  // 1. init interpreter
  ruby_init();
  ruby_init_loadpath();
  ruby_incpush(".");

  // 2. register c-based extension module
  Init_CDJukebox();

  // 3. register global instance
  register_global();

  // 4. run script
  // int state = eval_buffer("puts \"kamin babo\"\nprint $hardware");
  // int state = eval_file("/Users/yielding/test/rb/embed/embed.rb");

  g_box->set_unit(10);
  int state = eval_file("embed.rb");
  if (state) 
    printf("error\n");

  // 5. finalize
  // ruby_finalize();
  ruby_cleanup(0);
}
