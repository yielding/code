## 어떻게 쓰레드를 gracefully 종료시킬 것인가?
 1. interrupt
    - boost::thread는 정해진 interuption point에 있는 쓰레드를 interrupt할 수 있다.
    - interruption point
      + thread.join
      + thread.timed_join
      + this_thread.sleep
      + this_thread.intereruption_point
      + condition_variable.timed_wait
      + condition_variable.wait

 2. boost::thread_interrupted exception
    - 이 exception을 처리하지 않으면 여느 exception과 마찬가지로 처리된다.
  
 3. disable_interrupt
    - 이 객체에 의해 영향을 받고 있는 context에서는 interrupt가 무시된다

 4. conditional variable

boost/thread/libs/test를 본다. 깔끔한 예제들이 많이 있다.
그리고 thread::interrupt를 본다.
