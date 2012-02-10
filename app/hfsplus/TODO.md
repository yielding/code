TODO
====
 1. verification
    journal에서 carving한 내용을 윈도우와 비교해본다. (현재 조금 맞지 않음 다른 이유 중 하나는 catalog compare가 조금 다름)

 2. refactoring
    * virtual function 없애기

 3. implemantation
    BTree::read_empty_space 진짜 중요.!!!

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

 2. refactoring

 3. implemantation
    catalog_tree_node쪽 두 가지 다른 트리를 한번에 깔끔하게 처리하는 것
    remove_copy_if를 carve_node안에서 처리
DONE
====
 1. verification

 2. refactoring
    * read_leaf_record를 잘 정리해서 carv_tree_node 구현하기
     -> 내부의 try { } catch () {} 는 refactoring 가능
        -> exception처리까지 해서 가능하면 재활용할 수 있게 하는게 좋을 듯. 생각을 잘 하자....
    * BTree의 record type과 node type을 template으로 refactor하다. 
    * read_index_record
    * read_leaf_record

 3. implemantation
    * journal read
    * attribute->get_attribute 
    * EMFVolume->open
