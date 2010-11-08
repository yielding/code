#include "StdAfx.h"
#include "gps_automata.h"
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
gps_automata::gps_automata()
  : m_state(GPS_START),
    m_data(""),
    m_result("")
{
}

gps_automata::~gps_automata()
{
}

bool gps_automata::add(unsigned char in)
{
  if (m_state == GPS_START)
  {
    if (in == '$')
    {
      m_data  = in;
      m_state = GPS_CMD;
    }
  }
  else if (m_state == GPS_CMD)
  {
    m_data.push_back(in);
    if (m_data.length() < 6)
      return false;

    m_state = (m_data == "$GPGGA") ? GPS_DATA : GPS_START;
    if (m_state != GPS_DATA)
      m_data = "";
  }
  else if (m_state == GPS_DATA)
  {
    m_data.push_back(in);
    if (in == 0x0a)
    {
      m_result = m_data.substr(0, m_data.length()-2);
      m_data  = "";
      m_state = GPS_START;
      return true;
    }
  }

  return false;
}

string gps_automata::result()
{
  return m_result;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
collector_automata::collector_automata()
{
  m_result.reserve(17);
  m_result.clear();
  m_state  = START;
}

bool collector_automata::add(unsigned char in)
{
  if (m_state == START)
  {
    if (in == 2) 
    {
      m_state = DATA;
      m_result.push_back(in);
    }
  }
  else
  {
    m_result.push_back(in);
    if (m_result.length() == 17)
    {
      m_state = START;
      return true;
    }
  }

  return false;
}

string collector_automata::result()
{
  string res(0, 20);
  for (size_t i=0; i<m_result.length(); i++)
  {
    char buf[10] = { 0 };
    sprintf(buf, "%02X ", m_result[i]);
    res += string(buf);
  }

  m_result.clear();
  return res;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
