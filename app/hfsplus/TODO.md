TODO
====
 1. verification
    HFSPlus

 2. refactoring
    * virtual function 없애기

 3. implemantation
    * attribute->get_all_attributes
    * ExtentsTree::search_extents 구현
    * console에서 HFSPlusStr255 정상 출력하기
    * KeyStore객체를 만든다.
      이때동안 고려하지 않았던 unwrapCurve25519를 unwrapKeyForClass 안에 구현한다.

DOING
=====
 1. verification

 2. refactoring
    read_leaf_record를 잘 정리해서 carv_tree_node 구현하기
     -> 내부의 try { } catch () {} 는 refactoring 가능
        -> exception처리까지 해서 가능하면 재활용할 수 있게 하는게 좋을 듯. 생각을 잘 하자....

 3. implemantation
    BTree::read_empty_space 진짜 중요.!!!

DONE
====
 1. verification

 2. refactoring
    * BTree의 record type과 node type을 template으로 refactor하다. 
    * read_index_record
    * read_leaf_record

 3. implemantation
    * journal read
    * attribute->get_attribute 
    * EMFVolume->open
