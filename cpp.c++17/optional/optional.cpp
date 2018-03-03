#include <string>
#include <iostream>
#include <experimental/optional>

using namespace std;
using namespace std::experimental;

// optional can be used as the return type of a factory that may fail
optional<string> create(bool b) 
{
  if (b)
    return "Godzilla"s;
  else
    return {};
}

int main()
{
  cout << "create(false) returned "
       << create(false).value_or("empty") << '\n';

  // optional-returning factory functions are usable as conditions of while and if
  if (auto str = create(true))
    cout << "create(true) returned " << *str << '\n';

  return 0;
}
