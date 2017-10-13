#include "handle.h"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct my_trait
{
  using pointer = int*;

  static auto invalid() noexcept
  {
    return nullptr;
  }

  static auto close(pointer value) noexcept
  {
  }
};

int main(int argc, char *argv[])
{
  using my_handle = handle::utility::unique_handle<my_trait>;

  my_handle handle;


  return 0;
}
