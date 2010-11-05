#pragma once

#include <string>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class serial_automata
{
public:
  virtual bool add(unsigned char in) = 0;
  virtual string result() = 0;

protected:

};

class gps_automata: public serial_automata
{
public:
  enum { GPS_START, GPS_CMD, GPS_DATA };

public:
  gps_automata();
  ~gps_automata();

public:
  bool add(unsigned char in);
  string result();

private:
  string m_result;
  string m_data;
  int m_state;
};

class collector_automata: public serial_automata
{
public:
  enum { START, DATA };
public:
  collector_automata();
  bool add(unsigned char in);
  string result();

private:
  basic_string<unsigned char> m_result;
  int m_state;
};


////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
