#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <boost/filesystem.hpp>

using namespace std;
      namespace fs = boost::filesystem;

//vector<fs::path> get_files(fs::path& p)
void get_files(fs::path& p)
{
  vector<fs::path> paths;

  try
  {
    if (fs::exists(p))
    { 
      if (fs::is_regular_file(p))
      {
        cout << p << " size is " << fs::file_size(p) << '\n';
      }
      else if (fs::is_directory(p))
      {
        cout << p << " is a directory containing:\n";

        typedef vector<fs::path> vec;
        vec v;

        copy(fs::directory_iterator(p), fs::directory_iterator(), back_inserter(v));

        sort(v.begin(), v.end());
        auto it_end = v.end();
        for (auto it=v.begin(); it != it_end; ++it)
          get_files(*it);
      }
      else
      {
        cout << p << " exists, but is neither a regular file nor a directory\n";
      }
    }
  }
  catch (const fs::filesystem_error& ex)
  {
    cout << ex.what() << '\n';
  }

  //return paths;
}

int main(int argc, char* argv[])
{
  if (argc < 2)
  {
    cout << "Usage: tut4 path\n";
    return 1;
  }

  fs::path p(argv[1]);
  get_files(p);


  return 0;
}
