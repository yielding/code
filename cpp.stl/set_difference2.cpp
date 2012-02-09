#include <algorithm>
#include <vector>
#include <string>
#include <iostream>
#include <boost/lambda/lambda.hpp>

using namespace std;

int main(int argc, char *argv[])
{
  using namespace boost::lambda;

  vector<string> s1, s2, s3;

  s1.push_back("leech1");
  s1.push_back("leech2");
  s1.push_back("leech3");

  s2.push_back("leech1");

  set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(), 
      back_inserter(s3));

  for_each(s3.begin(), s3.end(), cout << _1 << " ");

  cout << endl;
}
