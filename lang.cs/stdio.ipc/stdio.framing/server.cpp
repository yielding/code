#include "message_framing.hpp"

#include <iostream>
#include <print>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace app
{
  using namespace std;
  using namespace xplat::io;
  using namespace xplat::framing;

  auto main() -> int
  {
    try
    {
      StdioBinaryMode binary_mode;
      
      auto input  = FileDescriptor::stdin_fd();
      auto output = FileDescriptor::stdout_fd();
      
      // based on user selection we can prepare various type of Processor
      MessageServer server{input, output, make_unique<EchoProcessor>()};
      
      if (auto result = server.run(); !result)
      {
        cerr << format("Server error: {}\n", result.error().message());
        return 1;
      }
      
      return 0;
    }
    catch (const exception& e)
    {
      cerr << format("Exception: {}\n", e.what());
      return 1;
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
  return app::main();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////