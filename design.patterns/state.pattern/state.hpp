#pragma once

#include <string>

class Post;

class State
{
public:
  virtual auto request_review() -> State* = 0;

  virtual auto approve() -> State* = 0;

  virtual auto content(const Post& p) -> std::string = 0;
};
