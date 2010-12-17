#include <boost/circular_buffer.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition.hpp>
#include <boost/thread/thread.hpp>
#include <boost/thread/xtime.hpp>
#include <boost/progress.hpp>
#include <boost/bind.hpp>
#include <boost/noncopyable.hpp>
#include <string>
#include <iostream>

const unsigned long QUEUE_SIZE = 1000L;

using namespace boost;
using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace {
  xtime delay(int secs, int msecs=0, int nsecs=0)
  {
    boost::xtime xt;
    boost::xtime_get(&xt, boost::TIME_UTC);
    xt.sec += secs;

    return xt;
  }
}

mutex io_mutex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <class T>
class bounded_buffer: noncopyable
{
public:
  typedef circular_buffer<T> container_type;
  typedef typename container_type::size_type size_type;
  typedef typename container_type::value_type value_type;

  explicit bounded_buffer(size_type capacity) : m_unread(0), m_container(capacity) {}

  int count()
  {
    return m_unread;
  }

  void push_front(const value_type& item)
  {
    mutex::scoped_lock lock(m_mutex);
    // 집어넣으려면 공간이 생길때까지 기다려야 한다.
    m_while_full.wait(lock, 
        bind(&bounded_buffer::is_not_full, this));

    m_container.push_front(item);
    ++m_unread;
    lock.unlock();
    m_while_empty.notify_all();
  }

  void pop_back(value_type* pItem)
  {
    mutex::scoped_lock lock(m_mutex);

    // 빼내려면 하나이상 존재햐야 한다.
    cout << "comsumer waiting ..\n";
    m_while_empty.wait(lock, 
        bind(&bounded_buffer::is_not_empty, this));

    *pItem = m_container[--m_unread];
    lock.unlock();
    m_while_full.notify_all();
  }

private:
  bool is_not_empty() const { return m_unread > 0; }
  bool is_not_full() const  { return m_unread < m_container.capacity(); }

private:
  size_type m_unread;
  container_type m_container;
  mutex     m_mutex;
  condition m_while_empty;
  condition m_while_full;
};

template<class Buffer>
class Consumer
{
public:
  Consumer(Buffer* buffer) 
    : m_container(buffer) 
  {}

  void operator() ()
  {
    try
    {
      while (true)
      {
        m_container->pop_back(&m_item);
        {
          mutex::scoped_lock l(io_mutex);
          cout << "consumer: " << m_item << " count: " << m_container->count() << "\n";
        }
      }
    }
    catch (boost::thread_interrupted& e)
    {
      cout << "consumer interrupted\n";
    }
  }

private:
  typedef typename Buffer::value_type value_type;
  Buffer* m_container;
  value_type m_item;
};

template<class Buffer>
class Producer
{
public:
  Producer(Buffer* buffer, int init) 
    : m_container(buffer), m_init(init)
  {}

  void operator()()
  {
    /*
       TODO
       1) 쉬면서 데이타를 넣는다. 
       2) 적당한 시점에 신호를 받고 종료한다.
       */

    for (int i=m_init; ; ++i)
    {
      m_container->push_front(int(i));
      {
        mutex::scoped_lock l(io_mutex);
        cout << "producer: " << i << " count: " << m_container->count() << "\n";
      }
      thread::sleep(delay(1)); 
    }
  }

private:
  typedef typename Buffer::value_type value_type;
  Buffer* m_container;
  int m_init;
};

template<class Buffer>
void fifo_test(Buffer* buffer)
{
  for (int i=0; i<100; i++) buffer->push_front(i);

  Consumer<Buffer> consumer(buffer);
  thread consume(consumer);
  thread_group pool;
  for (int i=0; i<10; i++) 
    pool.create_thread(Producer<Buffer>(buffer, i*100));

  thread::sleep(delay(10)); 
  pool.interrupt_all();
  consume.interrupt();

  pool.join_all();
  consume.join();
}

int main(int /*argc*/, char* /*argv*/[])
{
  bounded_buffer<int> bb_int(QUEUE_SIZE);
  fifo_test(&bb_int);

  return 0;
}
