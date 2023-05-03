NOTES
=====
 0. C#에서 메모리 누수 문제는 c++보다 더 심각해질 수 있다.

    GC가 돌아가면 프로그래머는 자동으로 메모리가 해제될 것이라고 생각하지만
    메모리 관리가 복잡하다.

    자바보다 더 복잡하다.

 1. IDisposable
    The primary use of this interface is to release unmanaged resources.

 2. GC calls destructor only.
    GC does not know anything about IDisposable interface or Dispose

 3. "using" keyword automatically calls  
    'Dispose' method of IDisposable interface
     
    compiler also generates null check codes for 'using' keyword
