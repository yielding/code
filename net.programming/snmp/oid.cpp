// 다음은 루비로 짜본다.
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

template <typename Iterator>
void print(Iterator f, Iterator l, char* sep=" ")
{
  while (f != l) cout << *f++ << sep;

  cout << endl;
}

int main()
{
  vector<string> oids;
  vector<int> nums;
  vector<int> last;

  for (int i=0; i<5; i++)
  {
    char v[50] = { 0 };
    sprintf(v, "1.2.3.4.5.6.7.8.9.%d.1", 2);
    oids.push_back(v);
  }

  for (int i=0; i<5; i++)
  {
    char v[50] = { 0 };
    sprintf(v, "1.2.3.4.5.6.7.8.9.%d.1", 5);
    oids.push_back(v);
  }

  for (int i=0; i<10; i++)
  {
    string oid = oids[i]; 
    int num = atoi(oid.substr(oid.find_last_of('.')+1).c_str());
    nums.push_back(num);
  }

  print(nums.begin(), nums.end());

  sort(nums.begin(), nums.end());
  unique_copy(nums.begin(), nums.end(), back_inserter(last));
  cout << last.size();
}
