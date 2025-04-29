#include <print>
#include <cstdlib>
#include "uniq.format.context.hpp"
#include "uniq.output.format.context.hpp"
#include "uniq.codec.context.hpp"
#include "av.resource.hpp"
#include "frame.queue.hpp"

using namespace std;

// #include "av_resource.hpp" // 위 코드

void example() 
{
  av::UniqueFormatContext fmt_ctx;
  fmt_ctx.open_input("input.mp4");
  fmt_ctx.find_stream_info();

  auto raw = fmt_ctx.get(); // 필요시 raw pointer 접근
}

int main(int argc, char* argv[])
{
  auto home = getenv("HOME");
  println("Hello, my home is {}", home);

  return 0;
}

