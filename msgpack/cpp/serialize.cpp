#include <msgpack.hpp>
#include <string>
#include <iostream>
#include <sstream>

using namespace msgpack;
using namespace std;

int main()
{
  type::tuple<int, bool, string> src(1, true, "example");

  // serialize the object into the buffer.
  // any classes that implements write(const char*,size_t) can be a buffer.
  stringstream buffer;
  pack(buffer, src);

  // send the buffer ...
  buffer.seekg(0);

  // deserialize the buffer into object instance.
  string str(buffer.str());

  auto oh = unpack(str.data(), str.size());

  // deserialized object is valid during the object_handle instance is alive.
  auto deserialized = oh.get();

  // object supports ostream.
  cout << deserialized << endl;

  // convert object instance into the original type.
  // if the type is mismatched, it throws type_error exception.
  type::tuple<int, bool, string> dst;
  deserialized.convert(dst);

  // or create the new instance
  auto dst2 = deserialized.as<type::tuple<int, bool, string>>();

  return 0;
}
