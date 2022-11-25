fibonacci
=========
  * Threadpool과 Event 객체를 사용하는 가장 적절한 예제.
  * .NET 2.0에서 사용하는 방식 4.0은 Task Pool이 소개
  * 이 방식의 가장 큰 문제점은 동작모델이 "Fire and Forgot"
    pool속에 있는 객체의 상태 및 결과에 대해서 데이터를 받을 수 있는 쉬운 방법이 없다.
