# Node
  https://github.com/mruby/mruby/wiki/Building-your-Ruby-environment-and-accessing-it.

이 코드는 host cpp코드에서 루비를 실행시켜서 원하는 값을 뽑아내는 
간단한 예제.

host cpp => WiKiManager instance 생성 
         => WiKiManager instance의 함수 실행
         => 함수 내에서 host 함수 실행
         => 결과를 host cpp의 caller에 반환
