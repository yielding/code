#include <list>
#include <functional>
#include <iostream>

using namespace std;
          
template <class T> class subList  
{
public:
  subList(int* begin, int* end):l(begin,end)
  {}

  int sort() 
  {
    l.sort();
    return 1;
  }

  int display()
  {
    copy(l.begin(),l.end(),ostream_iterator<int>(cout," "));
    cout<<endl;
    return 1;
  }

private:
  list<T> l;
};


int main()
{
  int a1[] = {2,1,5,6,4};
  int a2[] = {11,4,67,3,14};
  subList<int> s1(a1,a1+5);
  subList<int> s2(a2,a2+5);
    
  // Build a list of subLists
    
  list<subList<int>, allocator<subList<int> > > l;
  l.insert(l.begin(),s1);
  l.insert(l.begin(),s2);

  // Sort each subList in the list
  for_each(l.begin(),l.end(),mem_fun_ref(&subList<int>::sort));
    
  // Display the contents of list
  for_each(l.begin(),l.end(),mem_fun_ref(&subList<int>::display));

  return 0;
}
