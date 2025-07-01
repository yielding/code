#include <iostream>
#include <memory>
#include <thread>
#include <chrono>

using namespace std;

class SelfDestruct : public enable_shared_from_this<SelfDestruct> 
{
public:
  void start() 
  {
    cout << "Starting task...\n";

    // 비동기 작업 시 자기 자신에 대한 참조 유지
    auto self = shared_from_this();

    thread([self]() {
      this_thread::sleep_for(chrono::seconds(2));
      cout << "Task completed.\n";

      // 이후 외부 참조가 없다면 객체는 소멸됨
    }).detach();
  }

  ~SelfDestruct() 
  {
    cout << "SelfDestruct object destroyed.\n";
  }
};

int main() 
{
  {
    auto obj = make_shared<SelfDestruct>();
    obj->start();
    this_thread::sleep_for(chrono::seconds(1));
    cout << "Main scope exiting...\n";
  }

  this_thread::sleep_for(chrono::seconds(3));

  return 0;
}
