#include <thread>

using namespace std;

int main(int argc, char *argv[])
{
  std::jthread j([](stop_token t) {
  });

  
  return 0;
}
