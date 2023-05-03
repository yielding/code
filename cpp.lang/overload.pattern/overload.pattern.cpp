#include <iostream>
#include <string>
#include <variant>

using namespace std;

template<class... Ts> struct overload : Ts... { 
  using Ts::operator()...; 
};

// template<class... Ts> overload(Ts...) -> overload<Ts...>;

int main(int argc, char* argv[])
{
  variant<int, float, string> var_data { "Hello" };

  visit(overload {
    [](const int& i)    { cout << "int: " << i;    },
    [](const float& f)  { cout << "float: " << f;  },
    [](const string& s) { cout << "string: " << s; }
    },
    var_data
  );

  return 0;
}
