#include "http_source.h"

#include <cstdlib>
#include <cassert>
#include <fstream>

int main(int argc, char const* argv[])
{
  using namespace std;

  ofstream out("www.boost.org-index.html"); 

  HTTPSource http;
  http.handshake("www.boost.org", "/", 60);
  streamsize n = http.read_all();
  if (n <=0)
    exit(EXIT_FAILURE);

  vector<char>& buf = http.read_buffer();
  assert(buf.size() == n);
  out.write(&*buf.begin(), buf.size());

  return 0;
}

