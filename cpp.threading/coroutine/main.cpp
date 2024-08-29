#include <coroutine>
#include <iostream>
#include <stdexcept>
#include <thread>

using namespace std;

auto switch_to_new_thread(jthread& out)
{
  struct awaitable
  {
    jthread* p_out;
    bool await_ready() { return false; }
    void await_suspend(coroutine_handle<> h)
    {
      jthread& out = *p_out;
      if (out.joinable())
        throw runtime_error("Output jthread parameter not empty");
      out = jthread([h] { h.resume(); });
      // Potential undefined behavior: accessing potentially destroyed *this
      // cout << "New thread ID: " << p_out->get_id() << '\n';
      cout << "New thread ID: " << out.get_id() << '\n'; // this is OK
    }
    void await_resume() {}
  };
  return awaitable{&out};
}

struct task
{
  struct promise_type
  {
    task get_return_object() { return {}; }
    suspend_never initial_suspend() { return {}; }
    suspend_never final_suspend() noexcept { return {}; }
    void return_void() {}
    void unhandled_exception() {}
  };
};

task resuming_on_new_thread(jthread& out)
{
  cout << "Coroutine started on thread: " << this_thread::get_id() << '\n';
  co_await switch_to_new_thread(out);
  // awaiter destroyed here
  cout << "Coroutine resumed on thread: " << this_thread::get_id() << '\n';
}

int main()
{
  jthread out;
  resuming_on_new_thread(out);

  return 0;
}