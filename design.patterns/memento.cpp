#include <iostream>
#include <vector>
#include <string>

using namespace std;

// 객체의 내부 상태가 외부적으로 저장되어야 이후에 객체가 그 상태를 복구할 수 있다.
// 객체의 캡슐화가 훼손되지 않아야 한다.
struct memento
{
  memento(int s)
  {
    state = s;
  }

  int state;
};

class originator
{
public:
  auto set_state(int s) -> void { state = s; }
  auto get_state() const { return state; }

  auto to_memento() -> memento 
  {
    return memento(state);
  }

  auto from_memento(memento const& m) -> originator&
  {
    state = m.state;

    return *this;
  }

  auto to_s() -> string
  {
    return "state of org: " + to_string(state);
  }

private:
  int state;
};

class care_taker
{
public:
  auto add_memento(const memento& m) -> care_taker&
  {
    mementos.push_back(m);
    return *this;
  }

  auto get_memento(int index) const -> memento
  {
    return mementos[index];
  }

private:
  vector<memento> mementos;
};

int main(int argc, const char *argv[])
{
  originator org;
  org.set_state(10);

  care_taker ct;
  ct.add_memento(org.to_memento());

  org.set_state(20);
  ct.add_memento(org.to_memento());

  cout << org.from_memento(ct.get_memento(1))
             .to_s();
  
  return 0;
}