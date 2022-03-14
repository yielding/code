#include <chrono>
#include <iostream>
#include <thread>

using namespace std;
using namespace chrono_literals;

int main() 
{
  // Let's create a jthread an pass it a stop token
  jthread jt0([](stop_token tk) {
    // print in a loop
    for (int i = 0; i < 10; i++)
    {
      cout << "Printing value: " << i << '\n';

      // Stop, if that has been requested
      if (tk.stop_requested()) 
      {
        cout << "Stopping the jthread!\n";
        return;
      }

      this_thread::sleep_for(1s);
    }
  });

  // Pause the main thread before cancelling the jthread
  this_thread::sleep_for(5s);

  // request for our jthread to stop
  jt0.request_stop();

  return 0;
}
