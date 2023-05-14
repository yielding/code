#include <iostream>
#include <string>
#include <vector>

using namespace std;

auto from_to = [](auto start, auto finish) 
{
  return [=]() mutable 
  {
    if (start < finish)
      return start++;

    throw runtime_error("complete");
  };
};

int main(int argc, char *argv[])
{
  auto range = from_to(0, 10);

  cout << range() << endl;
  cout << range() << endl;
  cout << range() << endl;

  return 0;
}
