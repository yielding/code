#include "stdafx.h"

#include "win32_threadpool.h"

#include <process.h>
#include <string>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace sys {

  threadpool::threadpool()
  {
    m_queue = NULL;

    m_max_thread = 0;
    m_def_thread = 0;
    m_cur_thread = 0;
    m_wait_thread = 0;
    m_jobs_in_queue = 0;
  }

  bool threadpool::initialize(int def_thread, int max_thread)
  {
    m_def_thread = def_thread;
    m_max_thread = max_thread;
    m_queue = ::CreateIoCompletionPort((HANDLE)INVALID_HANDLE_VALUE, NULL, 0, m_def_thread);
    if (m_queue == NULL) 
      return false;

    m_cur_thread = m_def_thread;

    unsigned thread_id;
    for (int i=0; i<m_def_thread; i++) 
      ::CloseHandle((HANDLE)::_beginthreadex(NULL, 0, work_thread, (LPVOID)this, 0, &thread_id));

    return true;
  }

  // 스레드 풀에서 대기(또는 작동) 중인 모든 스레드를 종료한다.
  void threadpool::shutdown()
  {
    long count = m_cur_thread;

    // 전체 작업 스레드를 종료하기 위해 작업 스레드 수만큼 completion packet을 전달한다.
    // completion packet: (OVERLAPPED*)0xFFFFFFFF
    for (long i=0; i<count; i++) 
      ::PostQueuedCompletionStatus(m_queue, 0, 0, (OVERLAPPED*)0xFFFFFFFF);

    // 최대 15초간 모든 스레드가 안전하게 종료하길 기다린다.
    for (int i=0; i<20; i++) 
    {
      if (m_cur_thread <= 0) 
        break;

      ::Sleep(1000);
    }

    ::CloseHandle(m_queue);
    m_queue = NULL;
  }

  bool threadpool::launch_with(wchar_t* msg)
  {
    if (m_jobs_in_queue > MAX_WAIT_JOB) 
      return false;

    // 대기중인 스레드가 없고(모든 스레드가 BUSY), 가동 스레드 수가 최대 스레드 
    // 수보다 적은 경우 가동 스레드 수를 늘인다. 
    if ((m_wait_thread == 0) && (m_cur_thread < m_max_thread))
    {
      ::InterlockedIncrement(&m_cur_thread);
      unsigned thread_id;
      CloseHandle((HANDLE)_beginthreadex(NULL, 0, work_thread, (LPVOID)this, 0, &thread_id));
    }

    if (PostQueuedCompletionStatus(m_queue, (DWORD)msg, 0, NULL)) 
    {
      ::InterlockedIncrement(&m_jobs_in_queue);
      return true;
    }

    return false;
  }

  unsigned __stdcall threadpool::work_thread(LPVOID param)
  {
    ::CoInitialize(0);

    threadpool* pThis = reinterpret_cast<threadpool*>(param);  
  
    unsigned long pN1;
    unsigned long pN2; 
    OVERLAPPED* pOverLapped;

    auto bLoop = true;
    while (bLoop) 
    {
      ::InterlockedIncrement(&pThis->m_wait_thread);
      auto retVal = ::GetQueuedCompletionStatus(pThis->m_queue, 
          &pN1, 
          &pN2, 
          &pOverLapped, 
          120 * 1000);
      ::InterlockedDecrement(&pThis->m_wait_thread);

      // 2분동안 대기 작업이 없는 경우 기본 스레드 수 이상으로 생성된 스레드를 종료한다.
      if (retVal == FALSE && GetLastError() == WAIT_TIMEOUT) 
      {
        if (pThis->m_cur_thread > pThis->m_def_thread) 
          bLoop = false;

        continue;
      }

      // remark completion mark
      if (pOverLapped == (OVERLAPPED*)0xFFFFFFFF) 
      {
        bLoop = false;
      } 
      else 
      {
        pThis->run((void*)pN1);
        ::InterlockedDecrement(&pThis->m_jobs_in_queue);
      }
    }

    ::InterlockedDecrement(&pThis->m_cur_thread);
    
    ::CoUninitialize();

    return 0; 
  }
  
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
