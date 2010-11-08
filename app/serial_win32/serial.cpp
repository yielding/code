#include "stdafx.h"
#include "serial_port.h"

#include <iostream>
#include <fstream>
#include <string>

using namespace std;

void log_to_file(string const& path, string const& data)
{
  std::ofstream out;
  out.open(path.c_str(), ios_base::out | ios_base::app);
  out << data << '\n';
}

int _tmain(int argc, _TCHAR* argv[])
{
  serial_port p;
  if (!p.open(3))
  {
    cout << "port open error\n";
    return 1;
  }

  while(1)
  {
    string s = p.read_line();
    //if (s.length() > 0)
      cout << s << endl;
  }

  return 0;
}