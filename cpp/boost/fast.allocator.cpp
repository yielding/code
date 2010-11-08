#include <boost/pool/pool.hpp>
#include <boost/pool/pool_alloc.hpp>

#include <list>

using namespace std;

int main (int argc, char const* argv[])
{
  for (int i=0; i<100; i+=1)
  {
    list<int, boost::fast_pool_allocator<int> >l;
    for (int j=0; j<10000; j++)
      l.push_back(j);
  }

  return 0;
}
