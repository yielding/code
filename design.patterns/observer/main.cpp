#include <print>
#include <string>
#include <vector>
#include <algorithm>
#include <mutex>
#include <shared_mutex>

using namespace std;

class observer
{
public:
  virtual ~observer() = default;
  virtual void notify(string const& msg) = 0;
};

class subject
{
public:
  void add_observer(observer* o)
  {
    unique_lock<shared_mutex> lock(mtx);
    observers.emplace_back(o);
  }

  void notify_observers(string const& msg)
  {
    unique_lock<shared_mutex> lock(mtx);
    for (auto& o : observers) if (o != nullptr) o->notify(msg);
  }

  void remove_observer(observer* o)
  {
    unique_lock<shared_mutex> lock(mtx);
    observers.erase(remove(observers.begin(), observers.end(), o), observers.end());
  }

private:
  vector<observer*> observers;
  mutable shared_mutex mtx;
};

class concreate_observer : public observer
{
public:
  explicit concreate_observer(string name) 
    : name(std::move(name))
  {}

  void notify(string const& msg) override
  {
    println("{} received: {}", name, msg);
  }

private:
  string name;
};

int main(int argc, char* argv[])
{
  subject s;

  concreate_observer o1("o1");
  concreate_observer o2("o2");
  s.add_observer(&o1);
  s.add_observer(&o2);

  s.notify_observers("Hello, World!");
  println("removing observer o1");
  s.remove_observer(&o1);

  println("sending another notification");
  s.notify_observers("Hello, World!2");

  return 0;
}
