#include <deque>
#include <list>
#include <vector>
#include <algorithm>
#include <numeric>

#include <iostream>
    
using namespace std;

class copyIntoBuckets
{
public:
  copyIntoBuckets (int d, vector< deque<unsigned int,
      allocator<int> >, allocator<int>  > & b, bool & f) 
    : divisor(d), buckets(b), flag(f) {}
  int divisor;
  vector< deque<unsigned int,allocator<int> >, allocator<int>  > & buckets;
  bool & flag;
  void operator () (unsigned int v)
  {
    int index = (v / divisor) % 10;
    //
    // Flag is set to true if any bucket other than zeroth is used.
    //
    if (index) flag = true; 
    buckets[index].push_back(v);
  }
};

#ifndef HPPA_WA
list<unsigned int,allocator<int> >::iterator listCopy (list<unsigned int,allocator<int> >::iterator c,
                                       deque<unsigned int,allocator<int> > & lst)
#else
list<unsigned int,allocator<int> >::iterator listCopy (list<unsigned int,allocator<int> >::iterator c,
                                       deque<unsigned int,allocator<int> > lst)
#endif
{
    return copy(lst.begin(), lst.end(), c);
}

void radixSort (list<unsigned int,allocator<int> > & values)
{
    bool flag   = true;
    int divisor = 1;
    
    while (flag)
    {
        vector< deque<unsigned int,allocator<int> >, allocator<int>  > buckets(10);
        flag = false;
        for_each(values.begin(), values.end(), 
            copyIntoBuckets(divisor, buckets, flag));
        accumulate(buckets.begin(), buckets.end(), values.begin(), listCopy);
        divisor *= 10;
        copy(values.begin(), values.end(), ostream_iterator<int,char,char_traits<char> >(cout, " "));
        cout << endl;
    }
}

int main ()
{
    cout << "Radix sort program"  << endl;

    int data[] = { 624, 852, 426, 987, 269, 146, 415, 301, 730, 78, 593 };
    list<unsigned int,allocator<int> > values(11);
    copy(data, data + 11, values.begin());
    radixSort(values);
    copy(values.begin(), values.end(), ostream_iterator<int,char,char_traits<char> >(cout, " "));
    cout << endl;

    cout << "End radix sort program" << endl;

    return 0;   
}





