#include <iostream>
#include <string>
#include <algorithm>
#include <boost/lambda/lambda.hpp>

using namespace std;
using namespace boost::lambda;

int main()
{
  string a = "12,345,678";

  cout << a << endl;
  a.erase(remove_if(a.begin(), a.end(), _1 == ','), a.end());
  cout << a << endl;
}
