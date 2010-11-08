#include <hello.h>

int main()
{
  Hello msg("ideathinking");

  return msg.say() != "ideathinking";
}
