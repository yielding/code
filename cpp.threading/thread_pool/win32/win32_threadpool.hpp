#pragma once

#include <windows.h>

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
namespace sys {

  class threadpool
  {
  public:
    enum { MAX_WAIT_JOB = 32 };

  public:
    threadpool();
    ~threadpool() {}

    threadpool(const threadpool&) = delete;

  public: // api
    bool initialize(int def_thread, int max_thread);
    void shutdown();
    bool launch_with(wchar_t* _query);

    // 현재 작업 스레드 수를 반환한다.
    long thread_count() const { return m_cur_thread; }

  protected:
    virtual void run(void* param) = 0;

  protected:
    static unsigned __stdcall work_thread(LPVOID param);

  private:
    HANDLE m_queue;        // I/O Completion Port Handle
    long m_max_thread;     // 최대 작업 스레드 수 
    long m_def_thread;     // 기본 작업 스레드 수 
    long m_cur_thread;     // 현재 생성된 작업 스레드 수 
    long m_wait_thread;    // 대기 중인 스레드 수 
    long m_jobs_in_queue;  // 큐에 들어 있는 Job 수 
  };

}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////