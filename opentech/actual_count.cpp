#include <boost/format.hpp>

#include <string>
#include <vector>
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct LineInfo
{
  LineInfo()
  {
    m_count  = 1;
    m_tid    = "-1";
  }

  LineInfo(string line)
  {
    m_count  = 1;
    m_time   = line.substr( 1, 8);
    m_sc     = line.substr(12, 7);
    m_rc     = line.substr(20, 8);
    m_tid    = line.substr(29, 8);
  }

  LineInfo(LineInfo const& rhs)
  {
    if (this != &rhs)
    {
      m_count  = rhs.m_count;
      m_time   = rhs.m_time;
      m_sc     = rhs.m_sc;
      m_rc     = rhs.m_rc;
      m_tid    = rhs.m_tid; 
    }
  }

  LineInfo& operator=(LineInfo const& rhs)
  {
    if (this != &rhs)
    {
      m_count  = rhs.m_count;
      m_time   = rhs.m_time;
      m_sc     = rhs.m_sc;
      m_rc     = rhs.m_rc;
      m_tid    = rhs.m_tid; 
    }

    return *this;
  }

  string to_s()
  {
    return boost::str(boost::format("[%s][%s][%s][%s][%3d]") 
        % m_time % m_sc % m_rc % m_tid % m_count);
  }

  friend bool operator < (LineInfo const& a, LineInfo const& b)
  {
    return a.m_tid < b.m_tid;
  }

  string m_sc;
  string m_rc;
  string m_time;
  string m_tid;
  int    m_count;
};

typedef vector<LineInfo> LineInfos;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
void compact(LineInfos& lis, LineInfos& out)
{
  sort(lis.begin(), lis.end());

  LineInfo prev = lis[0];
  for (size_t i=0; i<lis.size(); ++i)
  {
    if (lis[i].m_tid == prev.m_tid) 
    {
      prev.m_count++;
      continue;
    }

    out.push_back(prev);
    prev = lis[i];
  }
}

int main(int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << "usage actual_count file_name\n";
    return EXIT_FAILURE;
  }

  ifstream in(argv[1]);
  if (!in.good())
    return EXIT_FAILURE;

  LineInfos lis;
  string line;
  while (getline(in, line))
  {
    LineInfo p(line);
    lis.push_back(p);
  }

  LineInfos out;
  compact(lis, out);

  for (size_t i=0; i<lis.size(); ++i)
    cout << boost::str(boost::format("%-3d: %s\n") 
        % i % lis[i].to_s());

  cout << "\n";
  for (size_t i=0; i<out.size(); ++i)
    cout << boost::str(boost::format("%-3d: %s\n") 
        % i % out[i].to_s());

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
