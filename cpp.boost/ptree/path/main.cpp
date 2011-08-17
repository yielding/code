#include "json_write_ext.hpp"

int main(int argc, char const* argv[])
{
  using namespace boost::property_tree::json_parser;
  
  std::wstring name = L"이창하";

  std::wcout << create_escapes(name);

  return 0;
}
