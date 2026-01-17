// Simple lock manager demo with S, IS, IX, SIX, X and compatibility matrix
#include <array>
#include <chrono>
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <sstream>
#include <string>
#include <string_view>
#include <thread>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace core
{
  using namespace std;

  auto tid_str() -> string
  {
    ostringstream oss;
    oss << this_thread::get_id();
    return oss.str();
  }

  enum class lock_mode { IS=0, IX, S, SIX, X, count };

  constexpr array mode_names = { "IS"sv, "IX"sv, "S"sv, "SIX"sv, "X"sv };

  // compatibility[requesting][existing]
  constexpr array<array<bool, 5>, 5> compatibility = {{
    /* existing: IS    IX     S     SIX    X  */
    /* IS  */ {{ true,  true,  true,  true,  false }},
    /* IX  */ {{ true,  true,  false, false, false }},
    /* S   */ {{ true,  false, true,  false, false }},
    /* SIX */ {{ true,  false, false, false, false }},
    /* X   */ {{ false, false, false, false, false }}
  }};

  constexpr int max_locks = 32;

  struct held_lock
  {
    thread::id owner;
    lock_mode mode;
  };

  class resource
  {
  public:
    resource(const string_view name)
      : _name(name)
    {
    }

  public:
    auto acquire(const lock_mode mode) -> void
    {
      unique_lock lock(_mtx);

      _cv.wait(lock, [this, mode] { return is_compatible(mode); });

      if (_held.size() < max_locks)
      {
        _held.push_back({ this_thread::get_id(), mode });
        cout << "[" << tid_str() << "] Acquired "
             << mode_names[static_cast<int>(mode)]
             << " on " << _name
             << " (held=" << _held.size() << ")\n";
      }
      else
      {
        cerr << "lock table full\n";
      }
    }

    auto release_by_owner() -> void
    {
      lock_guard lock(_mtx);

      auto me = this_thread::get_id();
      auto it = _held.begin();

      while (it != _held.end())
      {
        if (it->owner == me)
        {
          cout << "[" << tid_str() << "] Released "
               << mode_names[static_cast<int>(it->mode)]
               << " on " << _name << "\n";
          it = _held.erase(it);
        }
        else
        {
          ++it;
        }
      }

      _cv.notify_all();
    }

  private:
    auto is_compatible(const lock_mode req) const -> bool
    {
      for (auto const& h : _held)
        if (!compatibility[static_cast<int>(req)][static_cast<int>(h.mode)])
          return false;

      return true;
    }

  private:
    string _name;
    mutex _mtx;
    condition_variable _cv;
    vector<held_lock> _held;
  };

  // Demo scenario:
  // Reader: acquire IS on table, then S on row, hold, then release
  // Writer: acquire IX on table, then X on row, hold, then release
  resource table_res{"table_users"};
  resource row_res{"row_42"};

  auto reader_thread() -> void
  {
    table_res.acquire(lock_mode::IS);
    this_thread::sleep_for(chrono::milliseconds(100));
    row_res.acquire(lock_mode::S);

    cout << "[" << tid_str() << "] Reader: reading...\n";
    this_thread::sleep_for(chrono::seconds(2));

    row_res.release_by_owner();
    table_res.release_by_owner();
  }

  auto writer_thread() -> void
  {
    // start slightly after reader to show interleaving
    this_thread::sleep_for(chrono::milliseconds(200));
    table_res.acquire(lock_mode::IX);
    this_thread::sleep_for(chrono::milliseconds(50));
    row_res.acquire(lock_mode::X);

    cout << "[" << tid_str() << "] Writer: writing...\n";
    this_thread::sleep_for(chrono::seconds(2));

    row_res.release_by_owner();
    table_res.release_by_owner();
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto main() -> int
{
  using namespace core;

  thread r1(reader_thread);
  thread r2(writer_thread);

  r1.join();
  r2.join();

  cout << "Done\n";

  return 0;
}
