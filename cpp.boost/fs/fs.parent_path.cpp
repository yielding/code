#include <boost/filesystem.hpp>

using namespace std;
      namespace fs = boost::filesystem;
  
int main(int argc, char const* argv[])
{
  string src = "/Users/yielding/xxx/aaa.txt";

  fs::path p(src); 
  auto res = p.remove_leaf() /= "bbb.txt";

  assert(res == string("/Users/yielding/xxx/bbb.txt"));

  fs::remove_all(res);
}
