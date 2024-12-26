#include "post.hpp"

#include <memory>
#include "draft.hpp"

using namespace std;

Post::Post()
{
  _state = std::make_unique<Draft>();
}

auto Post::content() const -> string
{
  return _state->content(*this);
}

auto Post::add_text(const string& text) -> void
{
  _content = text;
}

auto Post::request_review() -> void
{
  _state.reset(_state->request_review());
}

auto Post::approve() -> void
{
  _state.reset(_state->approve());
}