#include <hello.h>

int main()
{
  Hello msg("world");

  return msg.say() != "world";
}
