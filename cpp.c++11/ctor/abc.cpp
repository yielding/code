#include <iostream>
#include <string>

using namespace std;

class T
{
public:
  T() = default;
  T(const char* str) : s(str) {}
  T(const T&) = delete;
  T & operator=(const T&) = delete;

private:
  string s;
};

int main(int argc, const char *argv[])
{
  return 0;
}
