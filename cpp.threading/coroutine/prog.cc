#include <iostream>
#include <coroutine>

using namespace std;

struct generator 
{
  struct promise_type;
  using handle = coroutine_handle<promise_type>;
  struct promise_type 
  {
    int current_value;
    auto get_return_object() { return generator{handle::from_promise(*this)}; }
    auto initial_suspend()   { return suspend_always{}; }
    auto final_suspend()     { return suspend_always{}; }
    void unhandled_exception() { terminate(); }
    void return_void() {}
    auto yield_value(int value) 
    {
      current_value = value;
      return suspend_always{};
    }
  };

  bool move_next()    { return coro ? (coro.resume(), !coro.done()) : false; }
  int current_value() { return coro.promise().current_value; }

  generator(generator const&) = delete;
  generator(generator && rhs) : coro(rhs.coro) { rhs.coro = nullptr; }
  ~generator() { if (coro) coro.destroy(); }

private:
  generator(handle h) : coro(h) {}
  handle coro;
};

generator f() 
{ 
  co_yield 1; 
  co_yield 2; 
}

int main() 
{
  auto g = f();
  while (g.move_next()) 
    cout << g.current_value() << endl;
}

