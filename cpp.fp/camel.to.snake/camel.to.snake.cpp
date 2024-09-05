#include <iostream>
#include <cstdint>
#include <cassert>
#include <range/v3/all.hpp>

namespace rg = ranges;
namespace rv = ranges::views;

using namespace std;

int main(int argc, char* argv[])
{
  auto const s = string{"CamelCaseIsGone"};

  // chunk_by는 
  // 1. binary predicate를 인자로 받는다.
  // 2. 넘겨온 함수가 true를 리턴하는 동안 chunk의 크기를 늘려간다.
  auto result
    = s 
    | rv::chunk_by([](uint8_t, uint8_t b) { return islower(b); })
    | rv::join('_')
    | rv::transform([](char c) { return tolower(c); })
    | rg::to<string>
    ;

  assert(result == "camel_case_is_gone");

  return 0;
}

/*
int main(int argc, char* argv[])
{
  auto const s = string{"CamelCaseIsGone"};
  auto result
    = s 
    | rv::chunk_by([](uint8_t a, uint8_t b) { 
        cout << a << "," << b << endl;
        return islower(b); })
    | rv::join('_')
    | rg::to<string>
    ;

  cout << result <<endl;
  // assert(result == "camel_case_is_gone");

  return 0;
}
*/