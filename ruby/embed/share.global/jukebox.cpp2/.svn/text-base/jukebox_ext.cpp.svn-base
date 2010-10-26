#include "jukebox_ext.h"

#include "rice/Data_Type.hpp"
#include "rice/Constructor.hpp"

using namespace Rice;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace jukebox_ext
{
  VALUE g_jukebox;

  void register_jukebox_global(CDJukebox* box)
  {
    Data_Object<CDJukebox> obj(box);
    g_jukebox = obj.value();

    rb_global_variable(&g_jukebox);
    rb_define_variable("$jukebox", &g_jukebox);
  }

  void init(CDJukebox* p)
  {
    RUBY_TRY
    {
      Data_Type<CDJukebox> rb_cCDJukebox =
        define_class<CDJukebox>("CDJukebox")
        .define_constructor(Constructor<CDJukebox, int>())
        .define_method("seek", &CDJukebox::seek)
        .define_method("seek_time", &CDJukebox::avg_seek_time)
        .define_method("unit", &CDJukebox::unit)
        ;
    }
    RUBY_CATCH;

    register_jukebox_global(p);
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
