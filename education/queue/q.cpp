#include <iostream>
#include <queue>        // queue, priority_queue
#include <deque>
#include <string>

using namespace std;

int main(int argc, const char *argv[])
{
    deque<int> dq;

    for (int i=0; i<10; i++)
        dq.push_front(i);

    while (!dq.empty()) 
    {
        cout << dq.back() << " ";
        dq.pop_back();
    }

    return 0;
}

/*
int main(int argc, const char *argv[])
{
    priority_queue<int> pq;
    
    pq.push(10);
    pq.push(1);
    pq.push(3);
    pq.push(11);

    while (!pq.empty())
    {
        cout << pq.top() << " ";
        pq.pop();
    }
    
    return 0;
}
*/

/*
int main(int argc, const char *argv[])
{
    queue<int> q;

    for (int i=0; i<10; i++)
       q.push(i);

    while (!q.empty())
    {
        cout << q.front() << " ";
        q.pop();
    }

    
    return 0;
}
*/


/*
int main(int argc, const char *argv[])
{
    deque<int> dq;
    queue<int> q;
    priority_queue<int> pq;

    for (int i=0; i<10; i++)
    {
        dq.push_front(i);
        pq.push(i);
         q.push(i);
    }

    while (!pq.empty())
    {
        auto i = pq.top();
        cout << "pq member: " << i << endl;
        pq.pop();
    }

    while (!q.empty())
    {
        auto i = q.front();
        cout << "queue member: " << i << endl;
        q.pop();
    }

    while (!dq.empty())
    {
        auto i = dq.back();
        cout << "deque member: " << i << endl;
        dq.pop_back();
    }
    
    return 0;
}

// adapter pattern
// stack and queue are made of deque
*/
