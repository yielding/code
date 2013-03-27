TODO
====
  * refactoring
    - unix socket => socket
  
DOING
=====
  * refactoring
    - exchange 합치기

DONE
====
  * refactoring
    - usbmux\_response 없애기

  * 연결 안되는 현상 테스트 방안
    - socket을 하나만 만들어 본다. (python code)
    - windows에서 send할때 정확하게 어떤 패킷들이 만들어지는지 테스트한다.
      (완전한 비교..)
      : 결과 <string>45</string> => <integer>45</integer>

  * packet 캡쳐
    - 처음부터 이걸 했었어야 했다.

  * asio 기본 동작 이해 (1)

  * SocketRelay 이해 (1)

  * USBMux Protocol 이해
    version 0을 지원하지 않고 1만 지원하도록 한다. 
    python 코드에서 v1 부분만 따라간다.

NOT-DONE
========
  * usb-proxy 코드의 구조를 그대로 따라간다.  python code는 이해하는 수단으로
