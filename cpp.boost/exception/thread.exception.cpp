#include <boost/exception_ptr.hpp>
#include <boost/thread.hpp>
#include <boost/bind.hpp>

#include <string>
#include <iostream>

struct file_read_error: virtual boost::exception {};
typedef boost::error_info<struct tag_md_message, std::string> err_message;

using namespace std;
using namespace boost;

void do_work()
{
  this_thread::sleep(posix_time::seconds(2));

#if 1
  throw boost::enable_current_exception(file_read_error()) 
    << err_message("leech");
#endif
}

void worker_thread(boost::exception_ptr& error)
{
  try
  {
    do_work();
    error = boost::exception_ptr();
    this_thread::sleep(posix_time::seconds(5));
  }
  catch (...)
  {
    error = boost::current_exception();
  }
}

int main(int argc, char* argv[])
{
  boost::exception_ptr error;
  boost::thread t(boost::bind(worker_thread, boost::ref(error)));
  t.join();

  this_thread::sleep(posix_time::seconds(5));
  if (error)
  {
    if (string const* mi = boost::get_error_info<err_message>(*error))
      cout << *mi;

    cout << boost::diagnostic_information(*error);
  }

  return 0;
}
