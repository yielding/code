#include <iostream>
#include <utility>

using namespace std;

struct Good
{
  Good() {}

  Good(Good&&) noexcept       // 예외를 던지지 않는다
  {
    cout << "Non-throwing move constructor called\n";
  }

  Good(const Good&) noexcept  // 예외를 던지지 않는다
  {
    cout << "Non-throwing copy constructor called\n";
  }
};

struct Bad
{
  Bad() {}

  Bad(Bad&&)          // 예외를 던질 수 있다
  {
    cout << "Throwing move constructor called\n";
  }

  Bad(const Bad&)     // 예외를 던질 수 있다
  {
    cout << "Throwing copy constructor called\n";
  }
};

int main()
{
  Good g;
  Bad  b;

  // Good 이동 생성자는 예외를 던지지 않으므로, move(g)를 반환
  // 이동 생성자가 실행된다
  auto g2 = move_if_noexcept(g);

  // Bad 이동 생성자는 예외를 던질 수 있으므로, b를 반환
  // 복사 생성자가 실행된다
  auto b2 = move_if_noexcept(b);

  return 0;
}

