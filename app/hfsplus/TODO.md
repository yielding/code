TODO
====
 1. verification
    HFSPlus

 2. refactoring
    read_index_record
    read_leaf_record

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

 3. implemantation

DONE
====
 1. verification

 2. refactoring
    * BTree의 record type과 node type을 template으로 refactor하다. 

 3. implemantation
    * journal read
    * attribute->get_attribute 
    * EMFVolume->open

