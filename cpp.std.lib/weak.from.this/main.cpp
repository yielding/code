#include <print>
#include <memory>

using namespace std;

struct OldWay : enable_shared_from_this<OldWay>
{
  void print()
  {
    if (auto sp = shared_from_this())
      println("OldWay object is still alive");
  }
};

struct NewWay
{ 
  void print() {
    if (auto sp = std::weak_from_this(*this).lock()) {
      println("NewWay object is still alive");
    }
  }
};


int main(int argc, char* argv[])
{
  auto o1 = std::make_shared<OldWay>();
  o1->print();

  auto o2 = std::make_shared<NewWay>();
  o2->print();

  return 0;
}
