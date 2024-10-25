#include <functional>
#include <iostream>

using namespace std;

template <typename S, typename A>
class State 
{
public:
  using StateFunction = function<pair<A, S>(S)>;

  explicit State(StateFunction runState) 
    : runState(runState) {}

  auto run(S init_state) const -> pair<A, S> 
  {
    return runState(init_state);
  }

  // Function to apply a transformation on the value while threading the state.
  template <typename B>
  auto map(function<B(A)> f) const -> State<S, B> 
  {
    return State<S, B>([f, runState = this->runState](S state) {
      auto [value, newState] = runState(state);
      return make_pair(f(value), newState);
    });
  }

  // Function to apply a monadic transformation, returning a new state monad.
  template <typename B>
  auto flat_map(function<State<S, B>(A)> f) const -> State<S, B> 
  {
    return State<S, B>([f, runState = this->runState](S state) {
      auto [value, newState] = runState(state);
      return f(value).run(newState);
    });
  }

private:
  StateFunction runState;
};

int main() 
{
  // State type: int (state is an integer), value type: int (value is also an integer)
  State<int, int> addState([](int state) {
    return make_pair(state + 10, state + 1);
  });

  // Map to double the value
  auto double_value = addState.map<int>([](int value) {
    return value * 2;
  });

  // FlatMap to add a new state operation
  auto ndd_then_multiply = double_value.flat_map<int>([](int value) {
    return State<int, int>([value](int state) {
      return make_pair(value * 3, state + 2);
    });
  });

  // Running the monad chain with an initial state of 5
  auto result = add_then_multiply.run(5);

  cout << "Result: " << result.first << ", Final State: " << result.second << endl;

  return 0;
}
