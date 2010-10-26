#include "stdafx.h"
#include "app.h"
#include "jukebox_ext.h"

#include <iostream>

App* App::m_instance = 0;
/////////////////////////////////////////////////////////////////////////////
//
//
//
/////////////////////////////////////////////////////////////////////////////
App::App()
{
  std::cout << "app begins\n";

  m_box = new CDJukebox(123);
  jukebox_ext::init(m_box);
}

App::~App()
{
  // m_box는 GC가 지운다.
  // delete m_box;
  std::cout << "app is deleting\n";
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

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
