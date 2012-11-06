#include <iostream>
#include <queue>

using namespace std;

class Stack
{
public:
    Stack();
    
    void push(int v);
    void pop();
    int top();
    bool empty();
    
private:
    deque<int> _data;
};

Stack::Stack()
{
    
}

void Stack::push(int v)
{
    _data.push_back(v);
}

void Stack::pop()
{
    _data.pop_back();
    
}

bool Stack::empty()
{
    return _data.empty();
}

int Stack::top()
{
    return _data.back();
}

int main (int argc, char const *argv[])
{
    Stack s;
    s.push(10);
    s.push(20);
    
    while (!s.empty())
    {
        cout << s.top();
        s.pop();
        
    }
    
    return 0;
}