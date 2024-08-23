#include "draft.hpp"
#include "pending_review.hpp"

#include <iostream>

Draft::Draft()
{
  std::cout << "Draft is created\n";
}

auto Draft::request_review() -> State* 
{
  return new PendingReview();
}

auto Draft::approve() -> State* 
{
  return this;
}

auto Draft::content(const Post& p) -> std::string 
{
  return "";
}
