#include <iostream>

using namespace std;

template<class... Ts> 
struct overload : Ts... { using Ts::operator()...; };

template <typename T, typename U, typename V>
struct Triple { T t; U u; V v; };

int main(int argc, char* argv[])
{
  Triple ttt{ 10.0f, 90, std::string{"hello"}};

  visit(overload  {
    [](const int& i)    { cout << "int: " << i;    },
    [](const float& f)  { cout << "float: " << f;  },
    [](const string& s) { cout << "string: " << s; }
    },
    ttt
  );

  return 0;
}