#include <chrono>
#include <future>
#include <iostream>
#include <thread>
#include <vector>

using namespace std;


// NOTICE
// from https://modoocode.com/284#page-heading-4
// 이 예제는 promise / future를 thread의 동시 시작을 atomic하게 하기 위한 방법
//

void runner(shared_future<void> start) 
{
  start.get();
  cout << "출발!" << endl;
}

int main()
{
  promise<void> p;
  shared_future<void> start = p.get_future();

  thread t1(runner, start);
  thread t2(runner, start);
  thread t3(runner, start);

  // 참고로 cerr 는 std::cout 과는 다르게 버퍼를 사용하지 않기 때문에 
  // 터미널에 바로 출력.
  cerr << "준비...";
  this_thread::sleep_for(chrono::seconds(1));
  cerr << "땅!" << endl;

  p.set_value();

  t1.join(); t2.join(); t3.join();

  return 0;
}