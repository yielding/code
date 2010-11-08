#include <iostream>
#include <exception>

using namespace std;

template <typename T>
class Singleton
{
public:
  Singleton()
  {
    int offset = (int)(T*)1 - (int)(Singleton<T>*)(T*)1;

    for (int i=0; i<10; i++)
      cout << "offset: " << offset << "\n";

    ms_singleton = (T*)((int)this + offset);
  }

  ~Singleton()
  {
    ms_singleton = 0;
  }

  static T& GetSingleton()
  {
    return *ms_singleton;
  }

  static T* GetSingletonPtr()
  {
    return ms_singleton;
  }

private:
  static T* ms_singleton;
};

template <typename T> T* 
Singleton<T>::ms_singleton = 0;

class Example: public Singleton<Example>
{
public:
  Example()
  {
    cout << "Example ctor\n";
  }

  void Print()
  {
    cout << "print is called\n"
  }
};

int main(int argc, char const* argv[])
{
  Example::GetSingleton().Print();
  
  return 0;
}
