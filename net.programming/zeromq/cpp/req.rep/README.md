### REQ/REP

  - REP can only reply to the socket that sent the msg
  - REQ : must do send + recv    
    REP : must do recv + send
  - 위의 제약사항이 매우 제한적이지만, 강력한 경우도 있다.
  - remote procedure all or take distribution
    publisher <-> consumer : data distribution 
