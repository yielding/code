#pragma once

#include "state.hpp"

class Published : public State
{
public:
  Published();

  auto request_review() -> State* override;

  auto approve() -> State* override;

  auto content(const Post& p) -> std::string override;
};