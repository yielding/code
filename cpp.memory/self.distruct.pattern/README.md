### Self-Destruct Pattern
- self-destruct 패턴은 객체가 자기 자신의 생명주기를 내부에서 스스로 관리하며, 필요에 따라 안전하게 소멸하는 패턴입니다. 주로 비동기 처리, 타이머, 콜백, 네트워크 요청 등에서 객체의 소멸 시점을 제어할 필요가 있을 때 사용됩니다.
- 이 패턴은 shared_ptr, enable_shared_from_this, 그리고 weak_from_this와 함께 사용하여, 메모리 안전성과 생명주기 관리를 동시에 달성합니다.