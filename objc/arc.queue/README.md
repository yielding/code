objc에서는 exception을 쓰지 않는것이 바람직

setjmp, longjmp로 구현되었기 때문에 자칫 잘못하면 메모리 관리를 완전히 
망쳐버릴 수 있다. 

Cocoa에서의 대안은 NSError를 사용하는 것.

objc++를 사용하는 입장에서는 c++의 exception을 사용하고 싶은 충동이 든다.

이 글을 쓰는 시점에서의 생각은 objc가 언어적으로 c++만큼 성숙되지 못하므로

c++의 객체는 내부 component로 objc는 Cocoa의 인터페이스로 사용하는 것이 옳은
선택으로 보인다.
