#include <iostream>
#include <string>
#include <msgpack.hpp>

#include <boost/format.hpp>

using namespace std;
using namespace boost;
using namespace msgpack;

//
// NOTICE
// this is intrusive version
//
class User
{
public:
  User() {}

  User(string name, int age) : m_name(name) , m_age(age) {}

  auto to_s() -> string
  {
    return str(format("name: %s, age: %d\n") % m_name % m_age); 
  }

  MSGPACK_DEFINE(m_name, m_age);

private:
  string m_name;
  int m_age;
};

int main(int argc, char* argv[])
{
  User user("alice"s, 25);

  // NOTICE
  // sbuffer : simple buffer
  sbuffer buffer;
  packer<sbuffer> packer_(&buffer); // connect packer and sbuffer
  packer_.pack(user);               // pack things into sbuffer

  // now
  // buffer.data(), buffer.size() 
  // can be used to file.write, socket.send

  // msgpack::object_handle oh;
  auto oh = msgpack::unpack(buffer.data(), buffer.size());
  // msgpack::object obj
  auto obj = oh.get();

  User user2;
  obj.convert(user2);

  cout << user2.to_s();

  return 0;
}
