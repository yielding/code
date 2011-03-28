#include "app.h"
#include "jukebox_ext.h"

#include <iostream>

App* App::m_instance = 0;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
App::App()
{
#ifdef DEBUG
  std::cout << "app begins\n";
#endif

  m_box = new CDJukebox(123);

  // REMARK: ruby GC에 등록. 
  // 따라서, destructor에서 해제하면 안된다.
  jukebox_ext::init(m_box);
}

App::~App()
{
#ifdef DEBUG
  std::cout << "app is deleting\n";
#endif

  // REMARK: GC가 지우므로 destructor에서 해제하면 안된다.
  // delete m_box;
}

App* App::instance()
{
  if (m_instance == 0)
    m_instance = new App();

  return m_instance;
}

void App::delete_instance()
{
  if (m_instance != 0)
  {
    delete m_instance;
    m_instance = 0;
  }
}

void App::set_jukebox(int v)
{
  if (m_box != 0)
    m_box->set_unit(v);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
