#include <iostream>
#include <functional>
#include <utility>

using namespace std;

template <typename State, typename Value>
class StateMonad 
{
public:
  using StateFunc = function<pair<Value, State>(State)>;

  // Constructor that takes a state transformation function
  explicit StateMonad(StateFunc func) : stateFunc(func) {}

  // Function to run the StateMonad and extract the result
  // pair<Value, State> run(State initialState) const {
  auto run(State initialState) const 
  {
    return stateFunc(initialState);
  }

  template <typename Func>
  //auto flatMap(Func f) const -> StateMonad<State, decltype(f(declval<Value>()).run(declval<State>()).first)> {
  auto flatMap(Func f) const 
  {
    using NewValue = decltype(f(declval<Value>()).run(declval<State>()).first);

    return StateMonad<State, NewValue>([=, this](State state) {
      auto [value, newState] = this->run(state);
      return f(value).run(newState);
    });
  }

  // Helper function to create a StateMonad from a simple value
  static StateMonad pure(Value value) 
  {
    return StateMonad([=](State state) { return make_pair(value, state); });
  }

private:
  StateFunc stateFunc;
};

// Example of a state manipulation function
auto increment(int amount) -> StateMonad<int, int> 
{
  return StateMonad<int, int>([=](int state) {
    return make_pair(state + amount, state + amount);
  });
}

// Example of using the State Monad
int main() {
  // Create a StateMonad that adds 5 to the state
  StateMonad<int, int> addFive = increment(5);

  // Chain multiple stateful computations
  auto program = addFive.flatMap([](int result) {
    return increment(result * 2); // Increment by double the previous result
  });

  // Run the program starting with an initial state of 10
  auto [finalValue, finalState] = program.run(10);

  cout << "Final Value: " << finalValue << endl;  // Output: Final Value: 40
  cout << "Final State: " << finalState << endl;  // Output: Final State: 40

  return 0;
}
