#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

using namespace ranges;
using std::string, std::vector, std::cout;

struct product
{
  string name;
  double price;
};

// NOTICE
// 함수형 언어이지만, 기존 객체의 일부 instance만 취해서 계산할 수 있다.
//
int main(int argc, char* argv[])
{
  auto data = vector<product> { 
    { "apple", 1.48 }, { "bread", 3.5 }, { "milk", 1.69} 
  };

  // NOTICE default operation {} == plus
  auto value = accumulate(data , 0.0, plus{}, &product::price);
  assert(value == 6.67);

  return 0;
}