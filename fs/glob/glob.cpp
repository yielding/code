#include <iostream>
#include <string>
#include <vector>

#include <glob.h>

using namespace std;

auto glob(const string& pat) -> vector<string>
{
  glob_t glob_result;
  glob(pat.c_str(), GLOB_TILDE, NULL, &glob_result);

  vector<string> ret;
  for (auto i=0; i<glob_result.gl_pathc; ++i)
    ret.push_back(move(string(glob_result.gl_pathv[i])));

  globfree(&glob_result);

  return ret;
}

int main(int argc, char *argv[])
{
  auto res = glob("/Users/yielding/code/**/*") ;

  for (auto f: res) cout << f << endl;
  
  return 0;
}
