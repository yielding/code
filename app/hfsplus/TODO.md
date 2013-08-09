TODO
====
 1. verification

 2. refactoring

 3. implemantation
    + union 없애기
      - BPlistRepr참고해서 variant로 구현한다. (variant는 utree 참고)

    + attribute -> get\_all\_attributes
    + ExtentsTree::search\_extents 구현
    + console에서 HFSPlusStr255 정상 출력하기
    + compressed된 HFS 구현하기
    + HFSX 구현하기. (for OSX forensics)

DOING
=====
 1. verification
    + brute\_force 검증

 2. refactoring

 3. implemantation
    + 자소 분리된 한글 처리 (keyboard 캐쉬)

DONE
====
 1. verification
    journal에서 carving한 내용을 윈도우와 비교해본다. 
      (현재 조금 맞지 않음 다른 이유 중 하나는 catalog compare가 조금 다름)
      중요한 파일에 대해서는 같게 나옴

 2. refactoring
    + read\_leaf\_record를 잘 정리해서 carv\_tree\_node 구현하기
     -> 내부의 try { } catch () {} 는 refactoring 가능
     -> exception처리까지 해서 가능하면 재활용할 수 있게 하는게 좋을 듯. 생각을 잘 하자....

    + BTree의 record type과 node type을 template으로 refactor하다. 
    + read\_index\_record
    + read\_leaf\_record

 3. implemantation
    + KeyStore객체를 만든다.
      - 이때동안 고려하지 않았던 unwrapCurve25519를 unwrapKeyForClass 안에 구현한다.
        -> emlpart 파일 decrypt

    + 문자메시지 파일의 key carving
      - hardlink가 나오는 경우가 있었다. (sms.db -> iNodeXXXX) 이 경우 sms.db의 필드 내부의 hardlink 값을 읽어서
        이름을 완성한다.

    + jpeg이외의 다른 signature carving.
      - sqlite, plist : 끝을 알 수 없는 문제가 있다.

    + sms sqlite page carving in the unused area  
      - regex로 전화번호 pattern 찾기

    + curve25519 구현 
      - 추가 검증. 이런 검증이 어렵네

    + brute\_force 방법을 기존대로 구현하기.
    + read\_empty\_space는 btree의 사용하지 않는 노드를 carving하는 함수
      현재 테스트 이미지에는 데이타가 없음
      데이타가 하나도 없음 

    + journal read
    + attribute : get\_attribute 
    + EMFVolume : open
