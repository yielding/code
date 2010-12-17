#include <vector>
#include <iostream>

using namespace std;

int main()
{
  int a[] = { 1, 2, 3, 4, 5 };
  vector<int> v(a, a+5);

  v.erase(v.begin()+1);
  vector<int> v1(v);

  cout << v.size()  << endl;
  cout << v1.size() << endl;

  for (int i=0; i<v.size(); i++)
    cout << v[i] << " ";

  cout << endl;
}
