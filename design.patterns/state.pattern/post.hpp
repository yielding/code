#pragma once

#include "state.hpp"

#include <memory>

class Post
{
public:
  Post();

  [[nodiscard("This result moust be used in the app")]]
  auto content() const -> std::string;

  auto add_text(std::string const& text) -> void;

  auto request_review() -> void;

  auto approve() -> void;

public:
  std::string _content;

private:
  std::unique_ptr<State> _state;
};
