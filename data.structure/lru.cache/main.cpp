#include <print>
#include <cstdlib>
#include <list>
#include <unordered_map>
#include <optional>

using namespace std;

class LRUCache
{
public:
  LRUCache(uint32_t capacity) :_capacity{capacity} {}

  auto get(int key) -> optional<int>
  {
    if (_map.find(key) == _map.end())
      return nullopt;

    auto it = _map[key];
    auto value = it->second;
    _entries.erase(it);
    _entries.push_front({key, value});
    _map[key] = _entries.begin();

    return value;
  }

  auto put(int key, int value) -> void 
  {
    if (_map.find(key) != _map.end())
      _entries.erase(_map[key]);

    _entries.push_front({key, value});
    _map[key] = _entries.begin();
    if (_entries.size() > _capacity)
    {
      auto last = _entries.back();
      _map.erase(last.first);
      _entries.pop_back();
    }
  }

private:
  uint32_t _capacity;
  list<pair<int, int>> _entries;
  unordered_map<int, list<pair<int, int>>::iterator> _map;
};

auto main() -> int
{
  auto cache = LRUCache(2);

  cache.put(1, 10);
  cache.put(2, 20);
  println("get(1) = {} (expect 10)", cache.get(1).value());

  // put(3) should evict key 2 (least recently used)
  cache.put(3, 30);
  println("get(2) = {} (expect nullopt)", cache.get(2).has_value() ? "hit" : "miss");
  println("get(3) = {} (expect 30)", cache.get(3).value());

  // put(4) should evict key 1
  cache.put(4, 40);
  println("get(1) = {} (expect nullopt)", cache.get(1).has_value() ? "hit" : "miss");
  println("get(3) = {} (expect 30)", cache.get(3).value());
  println("get(4) = {} (expect 40)", cache.get(4).value());

  // update existing key
  cache.put(3, 99);
  println("get(3) = {} (expect 99)", cache.get(3).value());

  return 0;
}
