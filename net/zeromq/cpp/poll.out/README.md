## Polling(Pollout)

- pollout은 socket의 맞은편에서 데이터를 받을 수 있는지를 확인해준다.
- poll의 return이 0인 경우 맞은편에서 data를 받을 준비가 되어있지 않다.
```cpp
int rc = poll(items, 1, 0);
```
- poll의 3번째 인자의 값이 -1, 0, +va에 따라서 block시간이 결정된다.
