#pragma once

#include "gps_automata.h"
#include <string>
#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class serial_port
{
public:
  serial_port();
  ~serial_port();

  bool open(int port);
  bool close();

  bool   clear();
  string read_line();
  string get_line() { return read_line(); }
  string read_line2();
  int    read(char* buf, int max_lead);
  int    write(wchar_t* buf, int len);

private:
  int m_port;
  HANDLE m_comm;
  DCB m_dcb;
  vector<unsigned char> m_buffer;
  collector_automata m_automata;
  // gps_automata m_automata;
};
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
