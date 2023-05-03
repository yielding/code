#include <thread> 
#include <iostream> 
#include <atomic> 

using namespace std; 
const int MAX = 50000000; 

volatile int x, y; 
volatile int trace_x[MAX], trace_y[MAX]; 

void thread_x() 
{
  for (int i = 0; i < MAX; ++i) 
  { 
    x = i; 
    atomic_thread_fence(memory_order_seq_cst);
    trace_y[x] = y; 
  }
}

void thread_y() 
{ 
  for (int i = 0; i < MAX; ++i) 
  { 
    y = i; 
    atomic_thread_fence(memory_order_seq_cst);
    trace_x[y] = x; 
  }
} 

//
// REMARKS
// 이 예제는 아래의 설명에 나오는 그림이 없으면 정말 이해하기 힘들겠다.
//
// https://popcorntree.tistory.com/15?category=813523
//
int main() 
{ 
  int count = 0; 

  thread t1 { thread_x }; 
  thread t2 { thread_y }; 
  t1.join(); 
  t2.join(); 

  // 연속된 숫자가 있나 없나 검사 
  for (int i = 0; i < (MAX - 1); ++i) 
  { 
    if (trace_x[i] != trace_x[i + 1]) 
      continue; 

    int x = trace_x[i]; 
    if (trace_y[x] != trace_y[x + 1]) 
      continue; 

    // 이러면 두개의 스레드의 메모리 읽고쓰는 순서가 다른것. 
    if (trace_y[x] == i) 
      count++; 
  } 

  cout << "Number of Error = " << count << endl; 

  return 0;
}
