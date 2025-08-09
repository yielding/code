#include <iostream>
#include <functional>
#include <string>
#include <chrono>
#include <vector>
#include <algorithm>
#include <utility>

using namespace std;
using namespace std::chrono;

// C++26 Reader Monad with std::function
template <typename Env, typename T>
class Reader 
{
public:
  using ReaderFunction = function<T(Env)>;
  
  explicit Reader(ReaderFunction func) : func_(func) {}
  
  T run(Env env) const { return func_(env); }
  
  template <typename Func>
  auto map(Func f) const -> Reader<Env, decltype(f(declval<T>()))>
  {
    return Reader<Env, decltype(f(declval<T>()))>(
      [=, this](Env env) { return f(run(env)); }
    );
  }
  
  template <typename Func>
  auto flatMap(Func f) const 
  {
    using ResultType = decltype(f(declval<T>()).run(declval<Env>()));
    return Reader<Env, ResultType>(
      [=, this](Env env) { return f(this->run(env)).run(env); }
    );
  }

private:
  ReaderFunction func_;
};

// Template-based Reader without std::function overhead
template <typename Env, typename Func>
class TemplateReader 
{
public:
  explicit constexpr TemplateReader(Func func) : func_(func) {}
  
  constexpr auto run(Env env) const { return func_(env); }
  
  template <typename F>
  constexpr auto map(F f) const 
  {
    return TemplateReader<Env, decltype([func = this->func_, f](Env env) { 
      return f(func(env)); 
    })>([func = this->func_, f](Env env) { 
      return f(func(env)); 
    });
  }
  
  template <typename F>
  constexpr auto flatMap(F f) const 
  {
    return TemplateReader<Env, decltype([func = this->func_, f](Env env) { 
      return f(func(env)).run(env); 
    })>([func = this->func_, f](Env env) { 
      return f(func(env)).run(env); 
    });
  }

private:
  Func func_;
};

// Helper to create TemplateReader
template <typename Env, typename Func>
auto makeTemplateReader(Func func) {
  return TemplateReader<Env, Func>(func);
}

// Config struct
struct Config {
  string prefix;
  string suffix;
  int multiplier;
};

// Benchmark: std::function-based Reader
auto benchmarkStdFunctionReader(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0; // Prevent optimization
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    Reader<Config, int> reader([i](Config cfg) { 
      return i * cfg.multiplier; 
    });
    
    auto result = reader
      .map([](int x) { return x + 10; })
      .map([](int x) { return x * 2; })
      .map([](int x) { return x; })
      .run(config);
    
    sink = result; // Prevent optimization
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

// Benchmark: Template-based Reader
auto benchmarkTemplateReader(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0;
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    auto lambda = [i](Config cfg) { return i * cfg.multiplier; };
    auto reader = TemplateReader<Config, decltype(lambda)>(lambda);
    
    auto result = reader
      .map([](int x) { return x + 10; })
      .map([](int x) { return x * 2; })
      .map([](int x) { return x; })
      .run(config);
    
    sink = result;
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

// Benchmark: Direct computation (baseline)
auto benchmarkDirect(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0;
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    int x = i * config.multiplier;
    x = x + 10;
    x = x * 2;
    sink = x;
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

// Benchmark: Lambda composition
auto benchmarkLambdaComposition(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0;
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    auto f1 = [&config](int i) { return i * config.multiplier; };
    auto f2 = [](int x) { return x + 10; };
    auto f3 = [](int x) { return x * 2; };
    
    int result = f3(f2(f1(i)));
    sink = result;
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

// Benchmark: std::function chain
auto benchmarkStdFunctionChain(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0;
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    function<int(int)> f1 = [&config](int i) { return i * config.multiplier; };
    function<int(int)> f2 = [](int x) { return x + 10; };
    function<int(int)> f3 = [](int x) { return x * 2; };
    
    int result = f3(f2(f1(i)));
    sink = result;
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

// C++23/26 style with explicit recursion
template <typename Fn>
struct ModernReader {
  Fn fn;
  
  template <typename Func>
  constexpr auto map(Func f) const {
    return ModernReader<decltype([fn = this->fn, f](auto env) {
      return f(fn(env));
    })>{[fn = this->fn, f](auto env) {
      return f(fn(env));
    }};
  }
  
  template <typename Env>
  constexpr auto operator()(Env env) const {
    return fn(env);
  }
};

template <typename Fn>
ModernReader(Fn) -> ModernReader<Fn>;

auto benchmarkModernReader(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  volatile int sink = 0;
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    ModernReader reader{[i](Config cfg) { return i * cfg.multiplier; }};
    
    auto pipeline = reader
      .map([](int x) { return x + 10; })
      .map([](int x) { return x * 2; });
    
    int result = pipeline(config);
    sink = result;
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<nanoseconds>(end - start).count();
}

int main() 
{
  constexpr int iterations = 1'000'000;
  constexpr int warmup = 10'000;
  
  cout << "C++26 Reader Monad Performance Benchmark\n";
  cout << "=========================================\n";
  cout << "Iterations: " << iterations << "\n\n";
  
  // Warmup
  benchmarkDirect(warmup);
  benchmarkLambdaComposition(warmup);
  benchmarkTemplateReader(warmup);
  benchmarkStdFunctionReader(warmup);
  benchmarkStdFunctionChain(warmup);
  benchmarkModernReader(warmup);
  
  // Run benchmarks multiple times and take average
  constexpr int runs = 5;
  vector<pair<string, double>> results;
  
  auto runBenchmark = [runs, iterations](auto benchFunc, const string& name) {
    long total = 0;
    for (int i = 0; i < runs; ++i) {
      total += benchFunc(iterations);
    }
    return make_pair(name, static_cast<double>(total) / runs / iterations);
  };
  
  results.push_back(runBenchmark(benchmarkDirect, "Direct computation"));
  results.push_back(runBenchmark(benchmarkLambdaComposition, "Lambda composition"));
  results.push_back(runBenchmark(benchmarkTemplateReader, "Template Reader"));
  results.push_back(runBenchmark(benchmarkModernReader, "Modern Reader (deducing this)"));
  results.push_back(runBenchmark(benchmarkStdFunctionChain, "std::function chain"));
  results.push_back(runBenchmark(benchmarkStdFunctionReader, "std::function Reader"));
  
  // Sort by performance
  ranges::sort(results, [](const auto& a, const auto& b) { 
    return a.second < b.second; 
  });
  
  // Display results
  cout << "Average time per iteration (nanoseconds):\n";
  cout << "-----------------------------------------\n";
  
  double baseline = results[0].second;
  for (const auto& [name, time] : results) {
    cout << name << ": " << time << " ns";
    if (time == baseline) {
      cout << " (baseline)";
    } else {
      double ratio = time / baseline;
      cout << " (" << ratio << "x slower)";
    }
    cout << "\n";
  }
  
  cout << "\n";
  cout << "Performance Analysis:\n";
  cout << "--------------------\n";
  
  // Find specific results
  auto findResult = [&results](const string& name) {
    return ranges::find_if(results, [&name](const auto& r) { 
      return r.first == name; 
    })->second;
  };
  
  double directTime = findResult("Direct computation");
  double templateTime = findResult("Template Reader");
  double stdFuncTime = findResult("std::function Reader");
  
  cout << "Template Reader overhead: " << (templateTime - directTime) << " ns\n";
  cout << "std::function Reader overhead: " << (stdFuncTime - directTime) << " ns\n";
  cout << "std::function vs Template ratio: " << (stdFuncTime / templateTime) << "x\n";
  
  return 0;
}