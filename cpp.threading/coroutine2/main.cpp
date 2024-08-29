#include <coroutine>
#include <iostream>
#include <optional>

using namespace std;

template<movable T>
class Generator
{
public:
  struct promise_type
  {
    auto get_return_object() { return Generator{handle::from_promise(*this)}; }

    auto initial_suspend() noexcept -> suspend_always { return {}; }
    auto final_suspend() noexcept -> suspend_always { return {}; }
    auto yield_value(T value) noexcept -> suspend_always 
    {
      current_value = move(value);
      return {};
    }

    // Disallow co_await in generator coroutines.
    auto await_transform() = delete;
    [[noreturn]]
      auto unhandled_exception() { throw; }

    optional<T> current_value;
  };

  using handle = coroutine_handle<promise_type>;

  explicit Generator(const handle coroutine) :
    m_coroutine{coroutine}
  {}

  Generator() = default;
  ~Generator()
  {
    if (m_coroutine)
      m_coroutine.destroy();
  }

  Generator(const Generator&) = delete;
  Generator& operator=(const Generator&) = delete;

  Generator(Generator&& other) noexcept :
    m_coroutine{other.m_coroutine}
  {
    other.m_coroutine = {};
  }
  Generator& operator=(Generator&& other) noexcept
  {
    if (this != &other)
    {
      if (m_coroutine)
        m_coroutine.destroy();
      m_coroutine = other.m_coroutine;
      other.m_coroutine = {};
    }
    return *this;
  }

  // Range-based for loop support.
  class Iter
  {
  public:
    void operator++()
    {
      m_coroutine.resume();
    }
    const T& operator*() const
    {
      return *m_coroutine.promise().current_value;
    }
    bool operator==(default_sentinel_t) const
    {
      return !m_coroutine || m_coroutine.done();
    }

    explicit Iter(const handle coroutine) :
      m_coroutine{coroutine}
    {}

  private:
    handle m_coroutine;
  };

  Iter begin()
  {
    if (m_coroutine) m_coroutine.resume();

    return Iter{m_coroutine};
  }

  default_sentinel_t end() { return {}; }

private:
  handle m_coroutine;
};

template <integral T>
auto range(T first, const T last) -> Generator<T> 
{
  while (first < last)
    co_yield first++;
}

int main()
{
  for (const char i : range(65, 91))
    cout << i << ' ';

  cout << '\n';
}
