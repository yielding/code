### publish / subscribe 모델에서 알아야 할 것
- 구독하는 데이터에 적당. 즉, 중복되는 데이터가 많이 생성되는 경우   
  여러 데이터 소스 중 내가 원하는 것만 받아보고 싶은 경우에 사용
- conflate 옵션은 마지막 데이터만 보여준다 
- 모든 소스를 subscribe하기 위해서 

```cpp
socket_t sock(ctx, socket_type::sub);
sock.set(sockopt::subscribe, "");
```
- conflate가 set된 경우 multipart message가 지원되지 않는다.   
  (sub.last.cpp 참고)

```cpp
int main()
{
  return 0;
}

```
