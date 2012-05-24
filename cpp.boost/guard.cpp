#include <iostream>
#include <boost/shared_ptr.hpp>

using namespace std;

void deleter(uint8_t* p)
{
    cout << "deleting...\n";
    printf("%x\n", p);
    delete [] p;
}

int main(int argc, char const* argv[])
{
    uint8_t* x = new uint8_t[10];

    boost::shared_ptr<uint8_t> ptr(x, std::ptr_fun(deleter));
 // boost::shared_ptr<T> ptr(new T, std::mem_fun_ref(&T::deleter));
    cout << "exiting ...\n";
    printf("%x\n", x);
    
    return 0;
}
