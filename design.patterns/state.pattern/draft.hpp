#pragma once

#include "state.hpp"

class Draft : public State
{
public:
  Draft();

  auto request_review() -> State* override;

  auto approve() -> State* override;

  auto content(const Post& p) -> std::string override;
};
