#include "get_pid.h"

#include <sstream>
#include <thread>

using namespace std;

string get_pid()
{
  stringstream ss;
  ss << this_thread::get_id();

  return ss.str();
}
