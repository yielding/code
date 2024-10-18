#include <iostream>
#include <functional>
#include <string>

using namespace std;

// Reader class
template <typename Env, typename T>
class Reader 
{
public:
  // Define the type of the function that Reader holds
  using ReaderFunction = function<T(Env)>;

  // Constructor: takes a function from Env to T
  explicit Reader(ReaderFunction func) : func_(func) {}

  // Run the Reader by providing an environment
  T run(Env env) const {
    return func_(env);
  }

  // Functor: fmap / map function
  template <typename Func>
  auto map(Func f) const -> Reader<Env, decltype(f(declval<T>()))> 
  {
    return Reader<Env, decltype(f(declval<T>()))>(
      [=, this](Env env) { return f(run(env)); }
    );
  }

  // Monad: flatMap / bind function
  template <typename Func>
  auto flatMap(Func f) const -> decltype(f(declval<T>())) {
    using NewReader = decltype(f(declval<T>()));
    return NewReader(
      [=, this](Env env) { return f(this->run(env)).run(env); }
    );
  }

private:
  ReaderFunction func_;
};

// Sample environment configuration
struct Config {
  string prefix;
  string suffix;
};

// Sample function that returns a Reader
auto greet(const string& name) -> Reader<Config, string> 
{
  return Reader<Config, string>([name](Config config) {
    return config.prefix + name + config.suffix;
  });
}

int main() 
{
  Config config = {"Hello, ", "!"};

  // Create a Reader instance
  Reader<Config, string> reader = greet("Alice");

  // Run the Reader with a configuration environment
  string result = reader.run(config);
  cout << result << endl;  // Output: Hello, Alice!

  // Example of using map to transform the result
  auto shout = reader.map([](const string& str) { return str + "!!!"; });
  cout << shout.run(config) << endl;  // Output: Hello, Alice!!!

  return 0;
}
