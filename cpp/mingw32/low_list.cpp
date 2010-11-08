#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/format.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cstdlib>

using namespace std;
using boost::str;
using boost::format;

using boost::multi_index_container;
using namespace boost::multi_index;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool has_problem(string const& line)
{
  if (line.length() < 37)
    return false;

  char flag = line[38];
  return (flag == '>') || (flag == '<') || (flag == '?') || (flag == '=') ||
         (flag == ':') || (flag == ';');
}

struct Problem
{
  Problem(string line)
  {
    m_count = 1;
    m_time  = line.substr(1, 8);
    m_sc    = line.substr(12, 7);
    m_rc    = line.substr(20, 8);
    m_tid   = line.substr(29, 8);
    m_signal= line.substr(38, 1);
  }

  bool is_afternoon() const
  {
    int t = atoi(m_time.substr(0,2).c_str());
    return (t > 12);
  }

  bool operator < (Problem const& p)
  {
    return m_tid < p.m_tid;
  }

  string to_s()
  {
    return str(format("[%s][%s][%s][%s][%s][%5d]") 
        % m_time % m_sc % m_rc % m_tid % m_signal % m_count);
  }

  string m_sc;
  string m_rc;
  string m_signal;
  string m_time;
  string m_tid;
  int    m_count;
};

typedef vector<Problem> Problems;

struct rc {};
struct sc {};

typedef multi_index_container <
  Problem,
  indexed_by <
    ordered_unique     < tag<rc>, member<Problem, string, &Problem::m_rc> >,
    ordered_non_unique < tag<sc>, member<Problem, string, &Problem::m_sc> >
  >
> ProblemMap;

typedef ProblemMap::index<rc>::type cache_rc_index;
typedef ProblemMap::index<sc>::type cache_sc_index;

class ProblemCache
{
public:
  enum { CACHE_INSERT, CACHE_UPDATE };

public:
  ProblemCache()
    : m_rc_index(m_cache.get<rc>())
    , m_sc_index(m_cache.get<sc>())
  {}

  int culmulate(Problem const& prob)
  {
    cache_rc_index::iterator it = m_rc_index.find(prob.m_rc);
    if (it == m_rc_index.end())
    {
      m_cache.insert(prob);
      return CACHE_INSERT;
    }

    Problem tmp = *it;
    tmp.m_count += 1;
    tmp.m_time = prob.m_time;
    m_rc_index.replace(it, tmp);

    return CACHE_UPDATE;
  }

  void clear()
  {
    m_cache.clear();
  }

  size_t size()
  {
    m_cache.size();
  }

  Problems get_problems()
  {
    Problems probs;
    cache_rc_index::iterator it = m_rc_index.begin();
    while (it != m_rc_index.end())
      probs.push_back(*it++);

    return probs;
  }

private:
  ProblemMap m_cache;
  cache_rc_index& m_rc_index;
  cache_sc_index& m_sc_index;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char* argv[])
{
  if (argc != 2)
  {
    cout << "usage low_list file_name\n";
    return EXIT_FAILURE;
  }

  ifstream in(argv[1]);
  if (!in.good())
    return EXIT_FAILURE;

  string line;
  ProblemCache cache;
  while(getline(in, line))
  {
    if (has_problem(line))
    {
      Problem p(line);
      cache.culmulate(p);
    }
  }

  Problems probs = cache.get_problems();
  for (size_t i=0; i<probs.size(); ++i)
    cout << str(format("%-3d: %s\n") % i % probs[i].to_s());

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
