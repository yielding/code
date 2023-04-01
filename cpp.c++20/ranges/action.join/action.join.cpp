#include <iostream>
#include <vector>
#include <string>

#include <range/v3/algorithm/copy.hpp>
#include <range/v3/action/join.hpp>
#include <range/v3/view/all.hpp>

using namespace ::ranges;
using std::vector, 
      std::string,
      std::cout, std::endl;

int main()
{
  vector<string> strs = {"show","me","the","money"};

  auto flatCopy = strs | copy | action::join;

  std::cout << views::all(flatCopy) << "\n";

  vector<vector<int>> v = { {1, 2, 3, 4, 5}, {6, 7, 8, 9, 0} } ;

  auto flatCopy2 = v | copy | action::join;
    
  cout << view::all(flatCopy2) << endl;


  return 0;
}
