#include <iostream>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
// compare with boost::dynamic_bitset
//
////////////////////////////////////////////////////////////////////////////////
class bitset
{
public:
  enum { SHIFT = 5,
         BITSPERWORD = 32,
         MASK = 0x1F
  };

public:
  explicit bitset(int size)
  {
    int count = 1 + size / BITSPERWORD;
    m_vector = new int[count]; 
    for (int i=0; i<count; i++) m_vector[i] = 0;
  }

  ~bitset()         
  { 
    delete [] m_vector; 
  }

  void set(int i)   {        m_vector[i >> SHIFT] |=  (1 << (i & MASK)); }
  void clear(int i) {        m_vector[i >> SHIFT] &= ~(1 << (i & MASK)); }
  bool test(int i)  { return m_vector[i >> SHIFT] &   (1 << (i & MASK)); }

private:
  int* m_vector;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  bitset cache(100000);

  for (int i=0; i<10; i++)
    cache.set(i);

  for (int i=0; i<10000; i++)
    cout << (cache.test(i) ? "set" : "not set") << "\n";
}
