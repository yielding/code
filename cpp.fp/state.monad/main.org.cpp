#include <iostream>
#include <functional>
#include <utility>

using namespace std;

// State 모나드 클래스 정의
template <typename S, typename A>
class State 
{
public:
  // 상태에서 값을 계산하는 함수. 상태를 받아 (결과, 새로운 상태)를 반환
  using StateFunc = function<pair<A, S>(S)>;

private:
  StateFunc runState;

public:
  // 생성자: 상태 함수를 받아서 모나드를 만듦
  explicit State(StateFunc runStateFunc) : runState(runStateFunc) {}

  // 상태를 적용하여 (결과, 새로운 상태)를 반환
  auto run(S state) const -> pair<A, S> 
  {
      return runState(state);
  }

  // 상태 모나드의 바인드 함수 (bind 또는 >>= 역할)
  template <typename B>
  auto bind(function<State<S, B>(A)> f) const -> State<S, B> 
  {
    return State<S, B>([this, f](S state) {
      auto [a, newState] = this->run(state);  // 현재 모나드의 상태 실행
      return f(a).run(newState);              // 결과를 새로운 모나드에 전달
    });
  }

  // 상태 모나드의 리턴 함수 (모나드의 값을 리턴)
  static State<S, A> pure(A value) 
  {
    return State<S, A>([value](S state) { return make_pair(value, state); });
  }
};

// 상태를 가져오는 함수 (get)
template<typename S>
auto get() -> State<S, S> 
{
  return State<S, S>([](S state) { return make_pair(state, state); });
}

// 상태를 설정하는 함수 (put)
template<typename S>
auto put(S newState) -> State<S, void> 
{
  return State<S, void>([newState](S) { return make_pair(nullptr, newState); });
}

// 간단한 상태 변경 예시: 카운터를 1씩 증가시키는 예제
auto increment() -> State<int, int> 
{
  return get<int>().bind<int>([](int state) 
  {
    //return put<int>(state + 1).bind<int>([state](...) {
    auto&& s = put<int>(state + 1);
    return s.bind<int>([state](...) {
      return State<int, int>::pure(state);  // 증가 전 상태 반환
    });
  });
}

int main() 
{
  // 초기 상태는 0
  State<int, int> program = increment().bind<int>([](int prevState) {
      cout << "Previous state: " << prevState << endl;
      return increment();  // 두 번째 increment 호출
  });

  auto [result, finalState] = program.run(0);  // 초기 상태 0으로 시작

  cout << "Final result: " << result << endl;
  cout << "Final state: "  << finalState << endl;

  return 0;
}
