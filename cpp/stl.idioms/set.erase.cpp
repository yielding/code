#include <set>
#include <vector>
#include <iostream>
#include <iterator>

bool less_than_5(int i)
{
  return i < 5;
}

int main(int argc, char const* argv[])
{
  using namespace std;
  set<int> s;

  for (size_t i=0; i<10; i++)
    s.insert(i);

  for (auto it=s.begin(); it != s.end(); )
  {
    if (less_than_5(*it))
    {
      // in case of vector: it, in it++
      // very delicate and confusing
      s.erase(it++);
    }
    else
    {
      ++it;
    }
  }

  copy(s.begin(), s.end(), ostream_iterator<int>(cout, " "));

  return 0;
}
