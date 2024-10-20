#include <functional>
#include <iostream>

// State type to hold a function that takes a state and returns a new state and value.
template <typename S, typename A>
class State {
public:
    using StateFunction = std::function<std::pair<A, S>(S)>;

    // Constructor to initialize the state monad with a function.
    explicit State(StateFunction runFunction) : runFunction(runFunction) {}

    // Function to execute the state computation.
    std::pair<A, S> run(S initialState) const {
        return runFunction(initialState);
    }

    // Function to apply a transformation on the value while threading the state.
    template <typename B>
    State<S, B> map(std::function<B(A)> f) const {
        return State<S, B>([f, runFunction = this->runFunction](S state) {
            auto [value, newState] = runFunction(state);
            return std::make_pair(f(value), newState);
        });
    }

    // Function to apply a monadic transformation, returning a new state monad.
    template <typename B>
    State<S, B> flatMap(std::function<State<S, B>(A)> f) const {
        return State<S, B>([f, runFunction = this->runFunction](S state) {
            auto [value, newState] = runFunction(state);
            return f(value).run(newState);
        });
    }

private:
    StateFunction runFunction;
};

// Example usage
int main() {
    // State type: int (state is an integer), value type: int (value is also an integer)
    State<int, int> addState([](int state) {
        return std::make_pair(state + 10, state + 1);
    });

    // Map to double the value
    auto doubleValue = addState.map<int>([](int value) {
        return value * 2;
    });

    // FlatMap to add a new state operation
    auto addThenMultiply = doubleValue.flatMap<int>([](int value) {
        return State<int, int>([value](int state) {
            return std::make_pair(value * 3, state + 2);
        });
    });

    // Running the monad chain with an initial state of 5
    auto result = addThenMultiply.run(5);

    std::cout << "Result: " << result.first << ", Final State: " << result.second << std::endl;

    return 0;
}

//#include <iostream>
//#include <functional>
//#include <utility>
//
//using namespace std;
//
//template <typename State, typename Value>
//class StateMonad 
//{
//public:
//  using StateFunc = function<pair<Value, State>(State)>;
//
//  // Constructor that takes a state transformation function
//  explicit StateMonad(StateFunc func) : stateFunc(func) {}
//
//  // Function to run the StateMonad and extract the result
//  // pair<Value, State> run(State initialState) const {
//  auto run(State initialState) const 
//  {
//    return stateFunc(initialState);
//  }
//
//  template <typename Func>
//  //auto flatMap(Func f) const -> StateMonad<State, decltype(f(declval<Value>()).run(declval<State>()).first)> {
//  auto flatMap(Func f) const 
//  {
//    using NewValue = decltype(f(declval<Value>()).run(declval<State>()).first);
//
//    return StateMonad<State, NewValue>([=, this](State state) {
//      auto [value, newState] = this->run(state);
//      return f(value).run(newState);
//    });
//  }
//
//  // Helper function to create a StateMonad from a simple value
//  static StateMonad pure(Value value) 
//  {
//    return StateMonad([=](State state) { return make_pair(value, state); });
//  }
//
//private:
//  StateFunc stateFunc;
//};
//
//// Example of a state manipulation function
//auto increment(int amount) -> StateMonad<int, int> 
//{
//  return StateMonad<int, int>([=](int state) {
//    return make_pair(state + amount, state + amount);
//  });
//}
//
//// Example of using the State Monad
//int main() {
//  // Create a StateMonad that adds 5 to the state
//  StateMonad<int, int> addFive = increment(5);
//
//  // Chain multiple stateful computations
//  auto program = addFive.flatMap([](int result) {
//    return increment(result * 2); // Increment by double the previous result
//  });
//
//  // Run the program starting with an initial state of 10
//  auto [finalValue, finalState] = program.run(10);
//
//  cout << "Final Value: " << finalValue << endl;  // Output: Final Value: 40
//  cout << "Final State: " << finalState << endl;  // Output: Final State: 40
//
//  return 0;
//}
