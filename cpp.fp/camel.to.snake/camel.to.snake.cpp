#include <iostream>
#include <cstdint>
#include <cassert>
#include <range/v3/all.hpp>

namespace g = ranges;
namespace v = ranges::views;

using namespace std;

int main(int agc, char* agv[])
{
  auto const s = string{"CamelCaseIsGone"};

  // chunk_by는 
  // 1. binary predicate를 인자로 받는다.
  // 2. 넘겨온 함수가 true를 리턴하는 동안 chunk의 크기를 늘려간다.
  auto result
    = s 
    | v::chunk_by([](uint8_t, uint8_t b) { return islower(b); })
    | v::join('_')
    | v::transform([](char c) { return tolower(c); })
    | g::to<string>
    ;

  assert(result == "camel_case_is_gone");

  return 0;
}

/*
int main(int agc, char* agv[])
{
  auto const s = string{"CamelCaseIsGone"};
  auto result
    = s 
    | v::chunk_by([](uint8_t a, uint8_t b) { 
        cout << a << "," << b << endl;
        return islower(b); })
    | v::join('_')
    | g::to<string>
    ;

  cout << result <<endl;
  // assert(result == "camel_case_is_gone");

  return 0;
}
*/