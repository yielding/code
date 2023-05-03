// mojo.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include <iostream>
#include <cassert>
#include <algorithm>
#include <string>

#include "mojo.h"

using namespace std;


//
// move constructor 예제.
//
// 아래의 enabled<String>을 상속받는다는 것은 결국
// String이라는 데이타타입에  temporary, constant conversion operator를 넣는것과 동일한 의미

class String: public mojo::enabled<String> 
{
public:
  String()
  {}

  String(char* s)
    :data_(s)
  {}

  String(const String& rhs)
    :data_(rhs.data_)
  {}

  String(mojo::fnresult<String> src)
  {
    String& rhs = src;
    data_.swap(src.data_);
  }

  String(mojo::temporary<String> src) 
  {
    String& rhs = src.get();
    data_.swap(rhs.data_);
  }

private:
  string data_;
};

mojo::fnresult<String>
MakeString()
{
    String s="123";
    return s;
}

void Discriminate(mojo::constant<String> s)
{}

void 
Discriminate(mojo::temporary<String> s)
{}

void
Discriminate(String &res)
{}

int main()
{
  String s(MakeString());
  const String cs("cs");

  Discriminate(cs);
  Discriminate(s);
  Discriminate(MakeString());

  return 0;
}

/**  
class Y: public mojo::enabled<Y> {
public:
    // default constructor
    Y () {
        cout << "CREATING default-constructed object at " << this << '\n';
    }

    // source is a const value
    Y (const Y& rhs) {
        cout << "COPYING const value at " << &rhs 
             << " into object at " << this << '\n';
    }

    // source is a fnresult
    Y (mojo::fnresult<Y> src) {
        Y& rhs = src;
        cout << "MOVING mojo::fnresult<Y> at " << &rhs 
             << " into object at " << this << '\n';
    }

    // source is a temporary
    Y (mojo::temporary<Y> src) {
        Y& rhs = src.get();
        cout << "MOVING mojo::temporary<Y> at " << &rhs 
             << " into object at " << this << endl;
    }

   ~Y() {
        cout << "DESTROYING object at " << this << endl;
    }
};

const Y 
MakeConstY()
{
    return Y();
}

mojo::fnresult<Y> 
MakeY()
{
    //if (cout) return Y();
    Y x;
    return x;
}

void 
TakeConstY(const Y&)
{
}

void 
TakeY(Y&)
{
}

void 
Discriminate(mojo::temporary<Y>)
{
}

void 
Discriminate(mojo::constant<Y>) 
{
}

void 
Discriminate(Y& obj) 
{ 
    Discriminate(mojo::constant<Y>(obj)); 
}


////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////

int main()
{
    return 0;
}


//
// 이해를 하고보면 좋은 예제
//
int main()
{
    Y nonConstLValue;
    const Y constLValue;
    
    //Y y1(constLValue);
    //Y y2(nonConstLValue);
    //Y y3(MakeConstY());
    Y y4(MakeY());

    TakeConstY(Y());

    Discriminate(Y());            // calls Discriminate(mojo::temporary<Y>)
    Discriminate(MakeY());        // calls Discriminate(mojo::temporary<Y>)
    Discriminate(constLValue);    // calls Discriminate(mojo::constant<Y>)
    Discriminate(nonConstLValue); // calls Discriminate(Y&)
    //TakeConstY(MakeY());

    //TakeY(Y());
    //TakeY(MakeY());
    return 0;
}
**/
