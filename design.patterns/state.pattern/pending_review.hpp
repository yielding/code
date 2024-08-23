#pragma once

#include "state.hpp"

class PendingReview : public State
{
public:
  PendingReview();

  auto request_review() -> State* override;

  auto approve() -> State* override;

  auto content(const Post& p) -> std::string override;
};
