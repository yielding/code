#include <iostream>
#include <set>
#include <utility>
#include <string>

using namespace std;

struct cache_item
{
  cache_item(string p, long s) : path(p), size(s) {}

  bool operator<(cache_item const& rhs) const 
  {
    return path < rhs.path;
  }

  string path;
  long   size;
};

typedef std::set<cache_item> cache;

void pr(cache& s)
{
  cout << "contents of set:\n";
  for (auto e: s) cout << e.path << " ";
  cout << endl;
}

int main(int argc, const char *argv[])
{
  cache s;

  s.insert(cache_item("leech", 0));
  s.insert(cache_item("kamin", 100));
  s.insert(cache_item("gunlee", 200));
  s.insert(cache_item("leeks", 300));

  auto it = s.find(cache_item("kamin", 200));
  if (it != s.end())
  {
    cout << "found: " << it->path;
  }

  return 0;
}
