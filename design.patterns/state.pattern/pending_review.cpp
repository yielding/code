#include "pending_review.hpp"
#include "published.hpp"

#include <iostream>

PendingReview::PendingReview()
{
  std::cout << "PendingReview is created\n";
}

auto PendingReview::request_review() -> State*
{
  return this;
}

auto PendingReview::approve() -> State*
{
  return new Published();
}

auto PendingReview::content(const Post& p) -> std::string
{
  return "";
}
