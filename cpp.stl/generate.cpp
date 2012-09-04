#include <iostream>
#include <algorithm>
#include <vector>
#include <iterator>

using namespace std;

template <typename Container>
void p(Container const& v)
{
  copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
}

int main0()
{
  vector<int> v(10);

  int i=0; auto gen = [&i](){ i +=1; return i; };
  generate(v.begin(), v.end(), gen); 

  p(v);
  
  return 0;
}

int main1()
{
}

int main(int argc, const char *argv[])
{
  main1();

  return 0;
}
