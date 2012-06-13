#include "jukebox.h"
#include "ruby.h"

#include <boost/format.hpp>
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
CDJukebox::CDJukebox(int unit)
{
#ifdef DEBUG
  std::cout << "CDJukebox ctor\n";
#endif
  m_unit_id = unit;
}

CDJukebox::CDJukebox(CDJukebox const& rhs)
{
  if (this != &rhs)
  {
#ifdef DEBUG
    std::cout << "copy constructor is called\n";
#endif

    m_unit_id = rhs.m_unit_id;
  }
}

CDJukebox::~CDJukebox()
{
#ifdef _DEBUG
  std::cout << "CDJukebox dtor\n";
#endif
}

void CDJukebox::assign(int unit_id)
{
  m_unit_id = unit_id;
}

// 이 함수는 순수 native에서 ruby함수를 호출하므로 
// 좀 문제가 있어보인다.
void CDJukebox::progress(int percent)
{
  if (rb_block_given_p())
  {
    if (percent > 100) percent = 100;
    if (percent <   0) percent = 0;
    rb_yield(INT2FIX(percent));
  }
}

void CDJukebox::seek(int disc, int track)
{
  using namespace boost;
  using namespace std;
  
  cout << str(format("unit: %d (%d, %d)\n") % m_unit_id % disc % track);

  for (int p=0; p<100; p+=10)
    progress(p);

  cout << "end of seek\n";
}

int CDJukebox::unit()
{
  return m_unit_id;
}

int CDJukebox::set_unit(int unit)
{
  return m_unit_id = unit;
}

double CDJukebox::avg_seek_time()
{
  return 1.23;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
