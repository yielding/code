#ifndef __JUKEBOX_H__
#define __JUKEBOX_H__

class CDJukebox
{
public:
  CDJukebox(int unit);
  CDJukebox(CDJukebox const& rhs);
  ~CDJukebox();

  void assign(int unit_id);

  void seek(int disc, int track);

  int unit();
  int set_unit(int unit);
  double avg_seek_time();

private:
  int   m_statusf;
  int   m_request;
  void* m_data;
  char  m_pending;
  int   m_unit_id;
  void* m_stats;
};

#endif

////////////////////////////////////////////////////////////////////////////////
//
// Assign the newly created CDPLayer to a particular unit
//
////////////////////////////////////////////////////////////////////////////////
/*
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
*/

