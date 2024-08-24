#include "post.hpp"

#include <iostream>

int main(int argc, char* argv[])
{
  Post post;
  post.add_text("hi");

  std::cout << post.content();

  post.request_review();
  std::cout << post.content();

  post.approve();
  std::cout << post.content();

  return 0;
}
