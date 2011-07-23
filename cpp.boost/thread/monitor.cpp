#include <vector>
#include <iostream>
#include <boost/thread/condition.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/recursive_mutex.hpp>
#include <boost/thread/thread.hpp>

namespace 
{
  const int ITERS = 100;
  boost::mutex io_mutex;
}

template <typename Mutex>
class buffer_t
{
public:
  typedef typename Mutex::scoped_lock scoped_lock;

  buffer_t(int n)
    : p(0), c(0), full(0), buf(n)
  {}

  void send(int m)
  {
    scoped_lock lk(mutex);
    while (full == buf.size())
      cond.wait(lk);

    buf[p] = m;
    p = (p+1) % buf.size();
    ++full;
    cond.notify_one();
  }

  int receive()
  {
    scoped_lock lk(mutex);
    while (full == 0)
      cond.wait(lk);

    int i = buf[c];
    c = (c+1) % buf.size();
    --full;
    cond.notify_one();

    return i;
  }

  static buffer_t& get_buffer()
  {
    // REMARK singleton object
    static buffer_t buf(20);

    return buf;
  }

  static void do_sender_thread()
  {
    for (int n=0; n<ITERS; ++n)
    {
      {
        boost::mutex::scoped_lock lock(io_mutex);
        std::cout << "sending: " << n << std::endl;
      }

      get_buffer().send(n);
    }
  }

  static void do_receiver_thread()
  {
    for (int x=0; x < (ITERS/2); ++x)
    {
      int n = get_buffer().receive();
      {
        boost::mutex::scoped_lock lock(io_mutex);
        std::cout << "received: " << n << std::endl;
      }
    }
  }

private:
  Mutex mutex;
  boost::condition cond;
  unsigned int p, c, full;
  std::vector<int> buf;
};

template <typename Mutex>
void do_test(Mutex* dummy=0)
{
  typedef buffer_t<Mutex> buffer_type;

  // 
  // single buffer object creation
  //
  buffer_type::get_buffer();

  boost::thread thrd0(&buffer_type::do_sender_thread);
  boost::thread thrd1(&buffer_type::do_receiver_thread);
  boost::thread thrd2(&buffer_type::do_receiver_thread);

  thrd0.join();
  thrd1.join();
  thrd2.join();
}

void test_buffer()
{
  do_test<boost::mutex>();
  do_test<boost::recursive_mutex>();
}

int main()
{
  test_buffer();
  return 0;
}
