#include "stdafx.h"
#include <memory>
#include <iostream>
#include <stdio.h>

#include "scope_guard.h"

using namespace std;
using namespace utility::guard;

// closeIt이나 freeIt의 Dissmiss를 호출하지 않으면  scope_guard의 destructor가
// 호출 transaction의 commit의 효과를 얻을려면 마지막에 Dismiss를 호출한다.
// 단지 마지막에 자동으로 메모리를 해제하고 싶으면 ON_BLOCK_EXIT(Obj)메크로를
// 사용한다.
// 
// Dissmiss == Commit <---> Rollback
void test()
{
   void* buffer = malloc(1024);
   scope_guard freeIt = make_guard(free, buffer);

   FILE* topSecret = fopen("cia.txt", "rw");
   scope_guard closeIt = make_guard(fclose, topSecret);
}

/*
void decrement(int& x)
{
	--x;
}

void useResource(int &refCount)
{
	++refCount;
	scope_guard guard = make_guard(decrement, ByRef(refCount));

	cout << "1" <<  refCount << endl;
}
*/

// 객체의 멤버함수를 사용할 경우.
// friends_.push_back(&newFriend);
// scope_guard guard = MakeObjGuard(friends_, &UserCont::pop_back);
//    

int main()
{
    test();
	int refCount = 1;
	// useResource(refCount);
 
    return 0;
}