#include <iostream>
#include <vector>
#include <cassert>

#include <range/v3/all.hpp>

namespace v = ranges::views;
namespace g = ranges;

using namespace std;

struct Product
{
  string name;
  double price;
};

// NOTICE
//
int main(int argc, char* argv[])
{
  auto data = vector<Product> { 
    { "apple", 1.48 }, { "bread", 3.5 }, { "milk", 1.69} 
  };

  // NOTICE default operation {} == plus
  auto value = g::accumulate(data , 0.0, g::plus{}, &Product::price);
  assert(value == 6.67);

  return 0;
}