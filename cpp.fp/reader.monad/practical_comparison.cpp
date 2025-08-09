#include <iostream>
#include <optional>
#include <expected>
#include <functional>
#include <string>
#include <memory>
#include <variant>

using namespace std;

////////////////////////////////////////////////////////////////////////////////
//
// 1. Error Handling: expected vs Exception vs Custom Monad
//
////////////////////////////////////////////////////////////////////////////////

// Traditional C++ approach with exceptions
class DatabaseTraditional {
public:
  string query(const string& sql) {
    if (sql.empty()) 
      throw runtime_error("Empty query");
    if (sql.find("DROP") != string::npos) 
      throw runtime_error("Dangerous query");
    return "Result: " + sql;
  }
  
  void useDatabase() {
    try {
      auto result = query("SELECT * FROM users");
      cout << result << "\n";
    } catch (const exception& e) {
      cerr << "Error: " << e.what() << "\n";
    }
  }
};

// Modern C++23 with std::expected (already monadic)
class DatabaseExpected {
public:
  expected<string, string> query(const string& sql) {
    if (sql.empty()) 
      return unexpected("Empty query");
    if (sql.find("DROP") != string::npos) 
      return unexpected("Dangerous query");
    return "Result: " + sql;
  }
  
  void useDatabase() {
    auto result = query("SELECT * FROM users")
      .and_then([](const string& data) { 
        return expected<string, string>(data + " [processed]"); 
      })
      .transform([](const string& data) { 
        return "Final: " + data; 
      });
    
    if (result) {
      cout << *result << "\n";
    } else {
      cerr << "Error: " << result.error() << "\n";
    }
  }
};

// Over-engineered custom Either monad
template <typename L, typename R>
class Either {
  variant<L, R> data;
public:
  static Either left(L l) { return Either{L{std::move(l)}}; }
  static Either right(R r) { return Either{R{std::move(r)}}; }
  
  template <typename F>
  auto map(F f) -> Either<L, decltype(f(declval<R>()))> {
    if (holds_alternative<R>(data)) {
      return Either<L, decltype(f(declval<R>()))>::right(f(get<R>(data)));
    }
    return Either<L, decltype(f(declval<R>()))>::left(get<L>(data));
  }
  
  template <typename F>
  auto flatMap(F f) -> decltype(f(declval<R>())) {
    if (holds_alternative<R>(data)) {
      return f(get<R>(data));
    }
    return decltype(f(declval<R>()))::left(get<L>(data));
  }
  
  bool isRight() const { return holds_alternative<R>(data); }
  R getRight() const { return get<R>(data); }
  L getLeft() const { return get<L>(data); }
  
private:
  Either(variant<L, R> d) : data(std::move(d)) {}
};

////////////////////////////////////////////////////////////////////////////////
//
// 2. Dependency Injection: Simple vs Reader Monad
//
////////////////////////////////////////////////////////////////////////////////

// Traditional C++ with constructor injection
class ServiceTraditional {
  struct Config { string apiUrl; string apiKey; };
  Config config;
  
public:
  ServiceTraditional(Config cfg) : config(std::move(cfg)) {}
  
  string fetchData() {
    return "Data from " + config.apiUrl;
  }
};

// Over-engineered Reader Monad approach
template <typename Env, typename T>
class Reader {
  function<T(Env)> func;
public:
  Reader(function<T(Env)> f) : func(f) {}
  T run(Env env) const { return func(env); }
  
  template <typename F>
  auto map(F f) -> Reader<Env, decltype(f(declval<T>()))> {
    return Reader<Env, decltype(f(declval<T>()))>(
      [*this, f](Env env) { return f(run(env)); }
    );
  }
};

struct Config { string apiUrl; string apiKey; };

Reader<Config, string> fetchDataReader() {
  return Reader<Config, string>([](Config cfg) {
    return "Data from " + cfg.apiUrl;
  });
}

////////////////////////////////////////////////////////////////////////////////
//
// 3. Async Operations: Practical vs Monadic
//
////////////////////////////////////////////////////////////////////////////////

// Practical C++ with coroutines (C++20)
#include <coroutine>

template<typename T>
struct Task {
  struct promise_type {
    T value;
    Task get_return_object() { 
      return Task{coroutine_handle<promise_type>::from_promise(*this)}; 
    }
    suspend_never initial_suspend() { return {}; }
    suspend_never final_suspend() noexcept { return {}; }
    void return_value(T v) { value = std::move(v); }
    void unhandled_exception() {}
  };
  
  coroutine_handle<promise_type> h;
  T get() { return h.promise().value; }
};

Task<int> asyncComputation() {
  // Simulate async work
  co_return 42;
}

// Over-engineered Future Monad
template <typename T>
class Future {
  function<T()> computation;
public:
  Future(function<T()> comp) : computation(comp) {}
  
  T get() { return computation(); }
  
  template <typename F>
  auto map(F f) -> Future<decltype(f(declval<T>()))> {
    return Future<decltype(f(declval<T>()))>(
      [*this, f]() { return f(get()); }
    );
  }
  
  template <typename F>
  auto flatMap(F f) -> decltype(f(declval<T>())) {
    return f(get());
  }
};

////////////////////////////////////////////////////////////////////////////////
//
// Main: Comparison and Recommendations
//
////////////////////////////////////////////////////////////////////////////////

int main() {
  cout << "=== C++ Monad Usage: Practical Analysis ===\n\n";
  
  cout << "1. ERROR HANDLING\n";
  cout << "-----------------\n";
  cout << "✅ Use std::expected/optional (built-in monadic interface)\n";
  cout << "❌ Don't create custom Either/Maybe monads\n\n";
  
  cout << "2. DEPENDENCY INJECTION\n";
  cout << "-----------------------\n";
  cout << "✅ Use constructor injection or factory patterns\n";
  cout << "❌ Reader Monad is overkill for C++\n\n";
  
  cout << "3. ASYNC OPERATIONS\n";
  cout << "-------------------\n";
  cout << "✅ Use coroutines, std::future, or callbacks\n";
  cout << "❌ Custom Future/IO monads add complexity\n\n";
  
  cout << "4. STATE MANAGEMENT\n";
  cout << "-------------------\n";
  cout << "✅ Use classes with encapsulated state\n";
  cout << "❌ State Monad doesn't fit C++ idioms\n\n";
  
  cout << "=== RECOMMENDATIONS ===\n\n";
  
  cout << "WHEN TO USE MONADIC PATTERNS IN C++:\n";
  cout << "• std::optional for nullable values ✅\n";
  cout << "• std::expected for error handling ✅\n";
  cout << "• ranges with | operator for transformations ✅\n";
  cout << "• Simple functional composition for specific domains ✅\n\n";
  
  cout << "WHEN NOT TO USE:\n";
  cout << "• General application architecture ❌\n";
  cout << "• Team projects without FP expertise ❌\n";
  cout << "• Performance-critical code ❌\n";
  cout << "• When simpler alternatives exist ❌\n\n";
  
  cout << "PRAGMATIC APPROACH:\n";
  cout << "1. Use standard library monadic types (optional, expected)\n";
  cout << "2. Adopt monadic interfaces (.and_then(), .transform())\n";
  cout << "3. Don't implement full monad typeclasses\n";
  cout << "4. Focus on practical benefits, not theoretical purity\n\n";
  
  cout << "CONCLUSION:\n";
  cout << "C++ is not Haskell. Embrace C++ idioms and use monadic\n";
  cout << "concepts sparingly where they provide clear value.\n";
  
  return 0;
}