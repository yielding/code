#include <iostream>
#include <string>
#include <vector>
#include <tuple>

using namespace std;

enum class user_info_fields 
{
  uiName, uiEmail, uiRepu
};

using user_info = tuple<string,  // name
                        string,  // email
                        size_t>; // reputation

template <typename E>
constexpr auto
to_utype(E enumerator) noexcept 
{
  return static_cast<underlying_type_t<E>>(enumerator);
}

int main(int argc, char *argv[]) 
{
  user_info i{"lee", "lee@gmail.com", 12};

  auto val = get<to_utype(user_info_fields::uiEmail)>(i);
  cout << val;

  return 0;
}
