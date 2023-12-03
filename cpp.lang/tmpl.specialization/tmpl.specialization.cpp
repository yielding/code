#include <iostream>

using namespace std;

template <class T>
struct FImpl;

template<class T>
void f(T t) 
{
  FImpl<T>::f(t);
}

// 0. least specialized
template<class T>
struct FImpl 
{
  static void f(T t) { cout << "least specialized: " << t << endl; };
};

// 1. partially specialized
template<class T>
struct FImpl<T*> 
{
  static void f(T* t) { cout << "partially specialized for pointer: " << t << endl; };
};

// 2. fully specialized
template<>
struct FImpl<double> 
{
  static void f(double t) { cout << "full specialized for double: " << t << endl; };
};

// 3. fully specialized
template<>
struct FImpl<char> 
{
  static void f(char t) { cout << "full spefull specialized for char: "<< t << endl; };
};

int main(int argc, char* argv[])
{
  int a = 10;

  f( 1   );
  f( 'a' );
  f( 1.1 );
  f( &a  );

  return 0;
}
