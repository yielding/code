### publish / subscribe 모델에서 알아야 할 것

- conflate 옵션은 마지막 데이터만 보여준다 
- subscribe를 모두 하기 위해서는 

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

