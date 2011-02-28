#include "ruby.h"
#include "cdjukebox.h"

static VALUE cCDJukebox;
static VALUE g_juke_box;

static void progress(CDJukebox *rec, int percent);

// Helper function to free a vendor CDJukebox
static void cd_free(void *p)
{
  free_jukebox(p);
}

  void free_jukebox(CDJukebox* jb)
  {
    printf("free_jukebox\n");

    free(jb);
  }

////////////////////////////////////////////////////////////////////////////////
//
// Allocate a new CDJukebox object, wrapping the vendor's CDJukebox structure
//
////////////////////////////////////////////////////////////////////////////////
static VALUE cd_alloc(VALUE klass)
{
  CDJukebox *jukebox;
  VALUE obj;

  // Vendor library creates the Jukebox
  jukebox = new_jukebox();

  // then we wrap it inside a Ruby CDJukebox object
  obj = Data_Wrap_Struct(klass, 0, cd_free, jukebox);

  return obj;
}

  CDJukebox* new_jukebox()
  {
    printf("new_jukebox\n");
    CDJukebox* jb = (CDJukebox*)malloc(sizeof(CDJukebox));

    return jb;
  }

////////////////////////////////////////////////////////////////////////////////
//
// Assign the newly created CDPLayer to a particular unit
//
////////////////////////////////////////////////////////////////////////////////
static VALUE cd_initialize(VALUE self, VALUE unit)
{
  int unit_id;
  CDJukebox *jb;

  Data_Get_Struct(self, CDJukebox, jb);

  unit_id = NUM2INT(unit);
  assign_jukebox(jb, unit_id);

  return self;
}

// Copy across state (used by clone and dup).  For jukeboxes, we
// actually create a new vendor object and set its unit number from
// the old
static VALUE cd_init_copy(VALUE copy, VALUE orig)
{
  CDJukebox *orig_jb;
  CDJukebox *copy_jb;

  if (copy == orig)
    return copy;

  // we can initialize the copy from other CDJukeboxs or their
  // subclasses only

  if (TYPE(orig) != T_DATA ||
      RDATA(orig)->dfree != (RUBY_DATA_FUNC)cd_free) 
  {
    rb_raise(rb_eTypeError, "wrong argument type");
  }

  // copy all the fields from the original object's CDJukebox
  // structure to the new object

  Data_Get_Struct(orig, CDJukebox, orig_jb);
  Data_Get_Struct(copy, CDJukebox, copy_jb);
  MEMCPY(copy_jb, orig_jb, CDJukebox, 1);

  return copy;
}

// Seek to a given part of the track, invoking the progress callback
// as we go
static VALUE cd_seek(VALUE self, VALUE disc, VALUE track)
{
  CDJukebox *jb;
  Data_Get_Struct(self, CDJukebox, jb);
  jukebox_seek(jb,
      NUM2INT(disc), 
      NUM2INT(track),
      progress);

  return Qnil;
}

// Return the average seek time for this unit
static VALUE cd_seek_time(VALUE self)
{
  double tm;
  CDJukebox *jb;
  Data_Get_Struct(self, CDJukebox, jb);
  tm = get_avg_seek_time(jb);

  return rb_float_new(tm);
} 

static VALUE cd_unit(VALUE self)
{
  CDJukebox *jb;
  Data_Get_Struct(self, CDJukebox, jb);
  return INT2NUM(jb->unit_id);
}

void assign_jukebox(CDJukebox* jb, int unit_id)
{
  jb->unit_id = unit_id;
}

double get_avg_seek_time(CDJukebox* jb)
{
  printf("get_avg_seek_time\n");

  return 54.321;
}

// The progress callback yields to the caller the percent complete
static void progress(CDJukebox *rec, int percent)
{
  if (rb_block_given_p()) 
  {
    if (percent > 100) percent = 100;
    if (percent < 0) percent = 0;
    rb_yield(INT2FIX(percent));
  }
}

void jukebox_seek(CDJukebox *jb, int disc, int track,
    void (*done)(CDJukebox *jb, int percent))
{
  int p;
  for (p=0; p<100; p +=10)
  {
    // sleep(1);
    done(jb, p);
  }

  printf("seeking disc %d and track %d for player %d\n", disc, track, jb->unit_id);
}

void Init_CDJukebox()
{
  cCDJukebox = rb_define_class("CDJukebox", rb_cObject);
  rb_define_alloc_func(cCDJukebox, cd_alloc);

  rb_define_method(cCDJukebox, "initialize", cd_initialize, 1);
  rb_define_method(cCDJukebox, "initialize_copy", cd_init_copy, 1);

  rb_define_method(cCDJukebox, "seek", cd_seek, 2);
  rb_define_method(cCDJukebox, "seek_time", cd_seek_time, 0);
  rb_define_method(cCDJukebox, "unit", cd_unit, 0);


  g_juke_box = cd_alloc(cCDJukebox);
  rb_global_variable(&g_juke_box);
  rb_define_variable("$jukebox", &g_juke_box);
}
