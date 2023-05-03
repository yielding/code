#include <iostream>
#include <exception>
#include <future>
#include <thread>
#include <string>

using namespace std;

void worker(promise<string>* p)
{
  try
  {
    throw runtime_error("some error");
    p->set_value("hi");
  }
  catch (...)
  {
    p->set_exception(current_exception());
  }
}

int main(int argc, char* argv[])
{
  promise<string> p;
  future<string> data = p.get_future();

  thread t(worker, &p);

  data.wait();

  try
  {
    data.get();
  }
  catch (exception& e)
  {
    cout << "exception: " << e.what() << endl;
  }

  t.join();

  return 0;
}