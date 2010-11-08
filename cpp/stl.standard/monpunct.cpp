#include <string>

#include <locale>
#include <iostream>

int main ()
{
  using namespace std;

  locale loc;

  // Get a moneypunct facet
  const moneypunct<char,false>& mp = 
#ifndef _RWSTD_NO_TEMPLATE_ON_RETURN_TYPE
    use_facet<moneypunct<char,false> >(loc);
#else
  use_facet(loc,(moneypunct<char,false>*)0);
#endif

  cout << "Decimal point        = " << mp.decimal_point() << endl; 
  cout << "Thousands seperator  = " << mp.thousands_sep() << endl; 
  cout << "Currency symbol      = " << mp.curr_symbol() << endl; 
  cout << "Negative Sign        = " << mp.negative_sign() << endl; 
  cout << "Digits after decimal = " << mp.frac_digits() << endl; 

  return 0;
}

