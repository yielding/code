#include <boost/circular_buffer.hpp>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <string>
#include <vector>
#include <iostream>

const unsigned long QUEUE_SIZE = 1000L;

using namespace std;

mutex io_mutex;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <class T>
class BoundedBuffer
{
public:
  typedef boost::circular_buffer<T> container_type;
  typedef typename container_type::size_type  size_type;
  typedef typename container_type::value_type value_type;

  explicit BoundedBuffer(size_type capacity) 
    : m_unread(0), m_container(capacity) 
  {}

  BoundedBuffer(const BoundedBuffer&) = delete;

  int count()
  {
    return m_unread;
  }

  auto push_front(const value_type& item) -> bool
  {
    unique_lock<mutex> lock(m_mutex);
    // 집어넣으려면 공간이 생길때까지 기다려야 한다.
    m_while_full.wait(lock,
      [this] { return m_unread < m_container.capacity() || m_shutdown; });

    if (m_shutdown)
      return false;

    m_container.push_front(item);
    ++m_unread;
    lock.unlock();
    m_while_empty.notify_all();

    return true;
  }

  auto pop_back(value_type* pItem) -> bool
  {
    unique_lock<mutex> lock(m_mutex);

    // 빼내려면 하나이상 존재햐야 한다.
    cout << "comsumer waiting ..\n";
    m_while_empty.wait(lock, [this] { return m_unread > 0 || m_shutdown; }); 

    if (m_shutdown)
      return false;

    *pItem = m_container[--m_unread];

    lock.unlock();
    m_while_full.notify_all();

    return true;
  }

  void shutdown()
  {
    m_shutdown = true;
    m_while_full.notify_all();
    m_while_empty.notify_all();
  }

private:
  bool m_shutdown {false};
  size_type m_unread;
  container_type m_container;
  mutex     m_mutex;
  condition_variable m_while_empty;
  condition_variable m_while_full;
};

template<class Buffer>
class Consumer
{
public:
  Consumer(Buffer* buffer) 
    : m_container(buffer) 
  {}

  void operator()()
  {
    while (true)
    {
      auto res = m_container->pop_back(&m_item);
      if (!res) 
      {
        cout << "shutdown consumer\n";
        break;
      }

      {
        unique_lock<mutex> l(io_mutex);
        cout << "consumer: " << m_item << " count: " << m_container->count() << "\n";
      }
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
    /* TODO
    1) 쉬면서 데이타를 넣는다. 
    2) 적당한 시점에 신호를 받고 종료한다.
    */

    for (int i=m_init; ; ++i)
    {
      auto res = m_container->push_front(int(i));
      if (!res) 
      {
        cout << "shutdown Producer\n";
        break;
      }

      {
        unique_lock<mutex> l(io_mutex);
        cout << "producer: " << i << " count: " << m_container->count() << "\n";
      }

      this_thread::sleep_for(1s);
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
  // 1. prepare buffer
  for (int i=0; i<100; i++) buffer->push_front(i);

  // 2. prepare producers
  
  vector<thread> pool;
  Consumer<Buffer> consumer(buffer);
  thread consume(consumer);
  for (int i=0; i<10; i++)
    pool.emplace_back(Producer<Buffer>(buffer, i*100));

  this_thread::sleep_for(10s);

  // x. interrupts thread
  cout << "shutdwon from main thread\n";
  buffer->shutdown();
  
  // x. Joint the threads
  for (auto& t : pool) if (t.joinable()) t.join();
  consume.join();
}

int main(int /*argc*/, char* /*argv*/[])
{
  BoundedBuffer<int> bb_int(QUEUE_SIZE);
  fifo_test(&bb_int);

  return 0;
}
