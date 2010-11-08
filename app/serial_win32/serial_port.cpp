#include "StdAfx.h"
#include "serial_port.h"
#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  wchar_t* port_name[] = { 
    L"COM0", L"COM1", L"COM2", L"COM3", L"COM4", L"COM5", 
    L"COM6", L"COM7", L"COM8", L"COM9", L"COM10" 
  };
}

serial_port::serial_port()
{
  m_port = -1;
  m_comm = NULL;
  m_buffer.reserve(100);
}

serial_port::~serial_port()
{
  close();
}

bool serial_port::open(int port)
{
  m_port = port;
  close();

  m_comm = ::CreateFile(port_name[m_port],  // communication port string (COMX)
      GENERIC_READ | GENERIC_WRITE,         // read/write types
      0,                                    // comm devices must be opened with exclusive access
      NULL,                                 // no security attributes
      OPEN_EXISTING,                        // comm devices must use OPEN_EXISTING
      FILE_ATTRIBUTE_NORMAL,                 
      0);                                   // template must be 0 for comm devices

  if (m_comm == INVALID_HANDLE_VALUE) 
    return false;

  if (!SetCommMask(m_comm, EV_RXCHAR))
    return false;

  int const BUFF_SIZE = 4192;
  if (!SetupComm(m_comm, BUFF_SIZE, BUFF_SIZE)) 
    return false;
  
  clear();

  // NOTICE
  // blocking mode when ReadIntervalTimeout = 0
  COMMTIMEOUTS cto;
  cto.ReadIntervalTimeout        = 0;
  cto.ReadTotalTimeoutConstant   = 0;
  cto.ReadTotalTimeoutMultiplier = 0;

  if (::SetCommTimeouts(m_comm , &cto) == 0)
    return false;

  if (!GetCommState(m_comm, &m_dcb))
    return false;

  m_dcb.DCBlength = sizeof DCB;
  // m_dcb.BaudRate  = 9600; // 38600 for DGPS;
  m_dcb.BaudRate  = 115200;
  m_dcb.ByteSize  = 8;
  m_dcb.StopBits  = ONESTOPBIT;
  m_dcb.Parity    = NOPARITY;
  m_dcb.fDtrControl = DTR_CONTROL_DISABLE;

  if (!SetCommState(m_comm, &m_dcb))
    return false;

  return true;
}

bool serial_port::close()
{
  if (m_comm != NULL)
  {
    ::SetCommMask(m_comm, 0);
    clear();
    ::CloseHandle(m_comm);
    m_comm = NULL;
  }

  return true;
}

string serial_port::read_line()
{
  DWORD tick = GetTickCount();
  while (GetTickCount() - tick < 1000)
  {
    DWORD dwEvent = 0;
    if (!WaitCommEvent(m_comm, &dwEvent, NULL))
      break;

    DWORD dwRead = 0;
    do
    {
      unsigned char chRead;
      if (::ReadFile(m_comm, &chRead, 1, &dwRead, NULL))
      {
        bool res = m_automata.add(chRead);
        if (res)
          return m_automata.result();
      }
      else
      {
        return "";          
      }
    } 
    while(dwRead);
  }

  return "";
}

//
// 1. 블로킹 I/O가 절대 나쁜게 아니다.
// 2. 읽기가 가장 중요한 부분이다. 
//
int serial_port::read(char* buf, int to_read)
{
  if (m_comm == NULL) 
    return -1;

  DWORD size = 0;
  while (true)
  {
    DWORD dwEvent = 0;
    WaitCommEvent(m_comm, &dwEvent, NULL);
    if ((dwEvent & EV_RXCHAR) == EV_RXCHAR)
      break;

    DWORD errors;
    COMSTAT stat;
    ::ClearCommError(m_comm, &errors, &stat);
    if (!stat.cbInQue)
      return 0;

    if (!::ReadFile(m_comm, buf, to_read, &size, NULL))
      return 0;

    if (size == 0)
      ::Sleep(100);
  }

  return size;
}

int serial_port::write(wchar_t* buf, int len)
{
  if (m_comm == NULL) 
    return -1;

  DWORD size;
  ::WriteFile(m_comm, buf, len, &size, NULL);

  return size;
}

bool serial_port::clear()
{
  DWORD fs = PURGE_RXCLEAR | PURGE_TXCLEAR | PURGE_TXABORT | PURGE_RXABORT;

  return !::PurgeComm(m_comm, fs); 
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
