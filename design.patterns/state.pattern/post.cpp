#include "post.hpp"
#include "draft.hpp"

using namespace std;

Post::Post()
{
  _state.reset(new Draft());
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
