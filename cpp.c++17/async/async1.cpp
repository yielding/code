#include <iostream>
#include <future>

using namespace std;

void called_from_async()
{
  cout << "Async call" << endl;
}

int main(int argc, const char *argv[])
{
  future<void> result(async(called_from_async));

  cout << "Message from main." << endl;

  result.get();

  return 0;
}
