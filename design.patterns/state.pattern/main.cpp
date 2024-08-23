#include "post.hpp"

#include <iostream>

using namespace std;

int main(int argc, char* argv[])
{
  Post post;
  post.add_text("hi");

  cout << post.content();

  post.request_review();
  cout << post.content();

  post.approve();
  cout << post.content();

  return 0;
}
