#include "published.hpp"
#include "post.hpp"

#include <iostream>

Published::Published()
{
  std::cout << "Published is created\n";
}

auto Published::content(const Post& p) -> std::string
{
  return p._content;
}

auto Published::request_review() -> State*
{
  return this;
}

auto Published::approve() -> State*
{
  return this;
}
