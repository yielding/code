#include <iostream>
#include <functional>
#include <variant>
#include <vector>
#include <random>
#include <optional>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
// 1. Reader Monad - run(environment)
// 환경을 제공받아 계산 수행
//
////////////////////////////////////////////////////////////////////////////////
template <typename Env, typename T>
class Reader {
public:
  using Func = function<T(Env)>;
  Reader(Func f) : func(f) {}
  
  // 환경을 받아 실행
  T run(Env env) const { 
    return func(env); 
  }
  
private:
  Func func;
};

// 사용 예
void readerExample() {
  cout << "=== Reader Monad ===\n";
  
  struct Config { int multiplier; };
  
  Reader<Config, int> computation([](Config cfg) {
    return 42 * cfg.multiplier;
  });
  
  // run 시점에 환경 제공
  Config config1{2};
  Config config2{3};
  
  cout << "With config1: " << computation.run(config1) << "\n";  // 84
  cout << "With config2: " << computation.run(config2) << "\n";  // 126
}

////////////////////////////////////////////////////////////////////////////////
//
// 2. State Monad - run(initial_state)
// 초기 상태를 받아 계산 수행, (값, 새로운 상태) 반환
//
////////////////////////////////////////////////////////////////////////////////
template <typename S, typename A>
class State {
public:
  using Func = function<pair<A, S>(S)>;
  State(Func f) : func(f) {}
  
  // 초기 상태를 받아 실행
  pair<A, S> run(S initial_state) const {
    return func(initial_state);
  }
  
  // 편의 메서드들
  A eval(S initial_state) const {
    return run(initial_state).first;
  }
  
  S exec(S initial_state) const {
    return run(initial_state).second;
  }
  
private:
  Func func;
};

void stateExample() {
  cout << "\n=== State Monad ===\n";
  
  State<int, string> stateful([](int state) {
    return make_pair("Count: " + to_string(state), state + 1);
  });
  
  // run: 초기 상태로 시작
  auto [value1, newState1] = stateful.run(0);
  cout << "Value: " << value1 << ", New State: " << newState1 << "\n";
  
  // 다른 초기 상태로 실행
  auto [value2, newState2] = stateful.run(10);
  cout << "Value: " << value2 << ", New State: " << newState2 << "\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 3. IO Monad - unsafeRun() 또는 runIO()
// 부수효과를 가진 계산을 명시적으로 실행
//
////////////////////////////////////////////////////////////////////////////////
template <typename T>
class IO {
public:
  using Action = function<T()>;
  IO(Action action) : action(action) {}
  
  // "unsafe"라는 이름으로 부수효과 있음을 경고
  T unsafeRun() const {
    return action();
  }
  
private:
  Action action;
};

void ioExample() {
  cout << "\n=== IO Monad ===\n";
  
  IO<void> printAction([]() {
    cout << "Performing side effect!\n";
    return;
  });
  
  IO<int> readAction([]() {
    cout << "Enter a number: ";
    int n;
    cin >> n;
    return n;
  });
  
  // IO 액션은 unsafeRun을 호출할 때만 실행됨
  cout << "IO created but not executed yet...\n";
  printAction.unsafeRun();  // 여기서 실제 실행
}

////////////////////////////////////////////////////////////////////////////////
//
// 4. Writer Monad - run()
// 누적된 로그와 함께 결과 반환
//
////////////////////////////////////////////////////////////////////////////////
template <typename W, typename A>
class Writer {
public:
  Writer(A value, W log) : value(value), log(log) {}
  
  // 값과 로그를 함께 반환
  pair<A, W> run() const {
    return {value, log};
  }
  
private:
  A value;
  W log;
};

void writerExample() {
  cout << "\n=== Writer Monad ===\n";
  
  Writer<vector<string>, int> computation(
    42, 
    vector<string>{"Started", "Computed", "Finished"}
  );
  
  auto [result, logs] = computation.run();
  cout << "Result: " << result << "\n";
  cout << "Logs: ";
  for (const auto& log : logs) {
    cout << log << " ";
  }
  cout << "\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 5. Cont (Continuation) Monad - run(continuation)
// 계속(continuation) 함수를 받아 실행
//
////////////////////////////////////////////////////////////////////////////////
template <typename R, typename A>
class Cont {
public:
  using Func = function<R(function<R(A)>)>;
  Cont(Func f) : func(f) {}
  
  // continuation을 받아 실행
  R run(function<R(A)> continuation) const {
    return func(continuation);
  }
  
private:
  Func func;
};

void contExample() {
  cout << "\n=== Continuation Monad ===\n";
  
  Cont<string, int> computation([](function<string(int)> cont) {
    return cont(42);
  });
  
  // 다양한 continuation으로 실행
  string result1 = computation.run([](int x) {
    return "The answer is: " + to_string(x);
  });
  
  string result2 = computation.run([](int x) {
    return x > 40 ? "Big number!" : "Small number";
  });
  
  cout << "With cont1: " << result1 << "\n";
  cout << "With cont2: " << result2 << "\n";
}

////////////////////////////////////////////////////////////////////////////////
//
// 6. Parser Monad - parse(input) 또는 runParser(input)
// 입력 문자열을 받아 파싱 수행
//
////////////////////////////////////////////////////////////////////////////////
template <typename T>
class Parser {
public:
  using ParseResult = optional<pair<T, string>>;
  using Func = function<ParseResult(string)>;
  
  Parser(Func f) : func(f) {}
  
  // 입력을 받아 파싱 실행
  ParseResult runParser(const string& input) const {
    return func(input);
  }
  
private:
  Func func;
};

void parserExample() {
  cout << "\n=== Parser Monad ===\n";
  
  Parser<int> parseInt([](string input) -> optional<pair<int, string>> {
    if (input.empty()) return nullopt;
    
    size_t pos;
    try {
      int value = stoi(input, &pos);
      return make_pair(value, input.substr(pos));
    } catch (...) {
      return nullopt;
    }
  });
  
  // 다양한 입력으로 파서 실행
  auto result1 = parseInt.runParser("123abc");
  if (result1) {
    auto [value, rest] = *result1;
    cout << "Parsed: " << value << ", Remaining: '" << rest << "'\n";
  }
  
  auto result2 = parseInt.runParser("xyz");
  if (!result2) {
    cout << "Parse failed for 'xyz'\n";
  }
}

////////////////////////////////////////////////////////////////////////////////
//
// Main - 모든 예제 실행
//
////////////////////////////////////////////////////////////////////////////////
int main() {
  cout << "Why Monads Have 'run' Interface\n";
  cout << "================================\n\n";
  
  cout << "The 'run' interface serves different purposes:\n";
  cout << "1. Provide context/environment (Reader)\n";
  cout << "2. Supply initial state (State)\n";
  cout << "3. Trigger side effects (IO)\n";
  cout << "4. Extract accumulated data (Writer)\n";
  cout << "5. Apply continuation (Cont)\n";
  cout << "6. Process input (Parser)\n\n";
  
  readerExample();
  stateExample();
  ioExample();
  writerExample();
  contExample();
  parserExample();
  
  cout << "\n=== Summary ===\n";
  cout << "'run' separates the DESCRIPTION of computation from its EXECUTION.\n";
  cout << "This enables:\n";
  cout << "- Lazy evaluation\n";
  cout << "- Multiple executions with different contexts\n";
  cout << "- Composition before execution\n";
  cout << "- Controlled side effects\n";
  
  return 0;
}