#include <iostream>
#include <string>

#include <boost/uuid/detail/sha1.hpp>
#include <boost/format.hpp>

using namespace std; 
using namespace boost;

auto to_sha1(string s) -> string
{
  uuids::detail::sha1 sh;
  sh.process_bytes(s.c_str(), s.length());
  sh.

  unsigned int digest[5];
  sh.get_digest(digest);

  string r;
  for (auto i : digest)
    r += str(format("%08x") % i);

  return r;
}

int main(int argc, char *argv[])
{
  try   
  {
    cout << to_sha1("abcdefg") << endl;
  }
  catch(...)
  {
  }
  
  return 0;
}
