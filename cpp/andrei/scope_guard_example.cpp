// (C) Copyright Jonathan Turkanis 2004.
// Distributed under the Boost Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt.)

#include <iostream>
#include <boost/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/scope_guard.hpp>

void goodbye() { std::cout << "goodbye\n"; }

void crash() { throw 0; }

struct T 
{
  void goodbye() { std::cout << "goodbye\n"; }
};

//
// Expected output:
//      hello
//      goodbye
//      goodbye
//      goodbye
//      goodbye
//      goodbye
//    
int main()
{
  using namespace boost;

  T t;

  // Scope guard which invokes t.goodbye(). 
  scope_guard g1 = make_guard(boost::bind(&T::goodbye, t));

  // Scope guard which invokes goodbye(). 
  scope_guard g2 = make_guard(goodbye);

  // Scope guard which executes std::cout << "goodbye\n".
  scope_guard g3 = make_guard(
      std::cout << boost::lambda::constant("goodbye\n")
      );

  // Scope guard which invokes crash() in a try-catch block. 
  scope_guard g4 = safe_guard(crash);

  // Scope guard which invokes crash(). 
  scope_guard g5 = make_guard(crash);
  g5.dismiss(); // dismiss했으므로 g5에서 로드한 함수가 수행된다... (compansation function)

  // Scope guard which invokes goodbye(). 
  BOOST_SCOPE_GUARD(goodbye);

  // Same. (Shows that generated variable names are working.)
  BOOST_SCOPE_GUARD(goodbye);

  // Scope guard which invokes crash() in a try-catch block. 
  BOOST_SAFE_GUARD(crash);

  std::cout << "hello\n";
}
