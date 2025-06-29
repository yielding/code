#include <boost/smart_ptr/atomic_shared_ptr.hpp>
#include <boost/smart_ptr/make_shared.hpp>
#include <iostream>
#include <thread>

struct MyData 
{
  int value;
};

int main() 
{
  boost::atomic_shared_ptr<MyData> atomic_ptr;

  // 초기화
  atomic_ptr.store(boost::make_shared<MyData>(MyData{42}));

  // 읽기
  auto local = atomic_ptr.load();
  std::cout << "value = " << local->value << std::endl;

  // 쓰기
  atomic_ptr.store(boost::make_shared<MyData>(MyData{100}));
  local = atomic_ptr.load();
  std::cout << "value = " << local->value << std::endl;

  return 0;
}
