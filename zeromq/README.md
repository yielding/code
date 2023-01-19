# 용어
## Intermediaries and Proxies (p.45)
  - 0MQ는 decentralized intelligence를 추구하고 있지만 그렇다고 모든 entitry가 별개로 떨어져있지 않ㅇ다.
  - 즉, 중간자가 존재하는데 intermediation이라 불리는 기능을 수행하는
  - 문맥에 따라 proxy, queeue, forwarder, device, broker 등으로 불린다.
  - It's better to think of intermediaries as simple stateless message switches.

## multipart message
  - frame or message parts
  - zmq_msg_t 