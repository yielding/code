#include <print>
#include <cstdio>
#include <filesystem>

using namespace std;
 
int main()
{
  print("{0} {1} {2}!", "Hello", "c++", 23);

  const auto tmp {filesystem::temp_directory_path() / "test.txt"};

  if (auto stream = fopen(tmp.c_str(), "w"))
  {
    print("File: {}\n", tmp.string());
    print(stream, "File: {}\n", tmp.string());
    fclose(stream);
  }

  return 0;
}