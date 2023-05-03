

#include "stdafx.h"
#include "stk.h"

template<typename T>
void 
Stack<T>::push(T& elem)
{
    elems.push_back(elem);
}

template<typename T>
T
Stack<T>::pop()
{
    T tmp = elems.back();
    elems.pop_back();

    return tmp;
}

template<typename T>
Stack<T>::Stack()
{

}

