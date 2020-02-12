#include "http_source.hpp"

#include <iostream>

int main (int argc, char * const argv[]) 
{
  HTTPSource source;
  if (source.handshake("www.boost.org", 60))
    std::cout << "ok\n";

  return 0;
}
