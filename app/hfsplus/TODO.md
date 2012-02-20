TODO
====
 1. verification

 2. refactoring

 3. implemantation
    * union 없애기
      BPlistRepr참고해서 variant로 구현한다.

    * attribute->get_all_attributes
    * ExtentsTree::search_extents 구현
    * console에서 HFSPlusStr255 정상 출력하기
    * KeyStore객체를 만든다.
      이때동안 고려하지 않았던 unwrapCurve25519를 unwrapKeyForClass 안에 구현한다.
      -> emlpart 파일 decrypt

DOING
=====
 1. verification
    * brute_force 검증

 2. refactoring

 3. implemantation
    * jpeg이외의 다른 signature carving.

DONE
====
 1. verification
    journal에서 carving한 내용을 윈도우와 비교해본다. 
      (현재 조금 맞지 않음 다른 이유 중 하나는 catalog compare가 조금 다름)
      중요한 파일에 대해서는 같게 나옴

 2. refactoring
    * read_leaf_record를 잘 정리해서 carv_tree_node 구현하기
     -> 내부의 try { } catch () {} 는 refactoring 가능
        -> exception처리까지 해서 가능하면 재활용할 수 있게 하는게 좋을 듯. 생각을 잘 하자....
    * BTree의 record type과 node type을 template으로 refactor하다. 
    * read_index_record
    * read_leaf_record

 3. implemantation
    * brute_force 방법을 기존대로 구현하기.
    * read_empty_space는 btree의 사용하지 않는 노드를 carving하는 함수
      현재 테스트 이미지에는 데이타가 없음
      데이타가 하나도 없음 

    * journal read
    * attribute->get_attribute 
    * EMFVolume->open
