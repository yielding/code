#include <iostream>
#include <functional>
#include <string>
#include <chrono>
#include <vector>

using namespace std;
using namespace std::chrono;

// Reader class (동일한 구현)
template <typename Env, typename T>
class Reader 
{
public:
  using ReaderFunction = function<T(Env)>;
  
  explicit Reader(ReaderFunction func) : func_(func) {}
  
  T run(Env env) const { return func_(env); }
  
  template <typename Func>
  auto map(Func f) const 
  {
    return Reader<Env, decltype(f(declval<T>()))>(
      [*this, f](Env env) { return f(run(env)); }
    );
  }
  
  template <typename Func>
  auto flatMap(Func f) const {
    return Reader<Env, decltype(f(declval<T>()).run(declval<Env>()))>(
      [*this, f](Env env) { return f(this->run(env)).run(env); }
    );
  }

private:
  ReaderFunction func_;
};

// Template-based function composition (no Reader wrapper)
template <typename F>
struct Composer {
  F f;
  
  template <typename G>
  auto then(G g) const {
    return Composer<decltype([f = this->f, g](auto x) { return g(f(x)); })>{
      [f = this->f, g](auto x) { return g(f(x)); }
    };
  }
  
  template <typename T>
  auto operator()(T&& t) const {
    return f(std::forward<T>(t));
  }
};

struct Config {
  string prefix;
  string suffix;
  int multiplier;
};

// 벤치마크 함수들
auto processWithReader(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    Reader<Config, int> reader([i](Config cfg) { 
      return i * cfg.multiplier; 
    });
    
    auto result = reader
      .map([](int x) { return x + 10; })
      .map([](int x) { return x * 2; })
      .map([](int x) { return to_string(x); })
      .run(config);
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<microseconds>(end - start).count();
}

auto processWithOptimizedReader(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    auto lambda = [i](Config cfg) { return i * cfg.multiplier; };
    auto reader = OptimizedReader<Config, decltype(lambda)>(lambda);
    
    auto result = reader
      .map([](int x) { return x + 10; })
      .map([](int x) { return x * 2; })
      .map([](int x) { return to_string(x); })
      .run(config);
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<microseconds>(end - start).count();
}

auto processDirectly(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    int x = i * config.multiplier;
    x = x + 10;
    x = x * 2;
    string result = to_string(x);
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<microseconds>(end - start).count();
}

auto processWithLambdas(const int iterations) 
{
  Config config = {"Result: ", "!", 2};
  
  auto start = high_resolution_clock::now();
  
  for (int i = 0; i < iterations; ++i) {
    auto f1 = [](Config cfg, int i) { return i * cfg.multiplier; };
    auto f2 = [](int x) { return x + 10; };
    auto f3 = [](int x) { return x * 2; };
    auto f4 = [](int x) { return to_string(x); };
    
    string result = f4(f3(f2(f1(config, i))));
  }
  
  auto end = high_resolution_clock::now();
  return duration_cast<microseconds>(end - start).count();
}

int main() 
{
  const int iterations = 1000000;
  
  cout << "Performance Benchmark (μs for " << iterations << " iterations)\n";
  cout << "=========================================================\n";
  
  // Warm up
  processDirectly(1000);
  processWithLambdas(1000);
  processWithReader(1000);
  processWithOptimizedReader(1000);
  
  // Actual benchmarks
  vector<pair<string, long>> results;
  
  results.push_back({"Direct computation", processDirectly(iterations)});
  results.push_back({"Lambda composition", processWithLambdas(iterations)});
  results.push_back({"Optimized Reader (template)", processWithOptimizedReader(iterations)});
  results.push_back({"Reader Monad (std::function)", processWithReader(iterations)});
  
  // Sort by performance
  sort(results.begin(), results.end(), 
       [](const auto& a, const auto& b) { return a.second < b.second; });
  
  // Display results
  long baseline = results[0].second;
  for (const auto& [name, time] : results) {
    double ratio = static_cast<double>(time) / baseline;
    cout << name << ": " << time << " μs";
    if (time == baseline) {
      cout << " (baseline)";
    } else {
      cout << " (" << ratio << "x slower)";
    }
    cout << "\n";
  }
  
  return 0;
}