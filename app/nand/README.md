## TODO
  * NAND class 구현
    - NANDRemote
      : readPage

    - VFL
    
    - VSVFL

    - YAFTL

## DOING
  * NAND class 구현

## DONE
  * NAND class 구현
    - NANDImageSplitCEs

    - NANDImageFlat
      > read_page

      > CRTP?, polymorphic? => polymorphic!
        boost::fusion은 heterogeneous type을 위한 compile/runtime container, algoritm

  * PTreeParser 버그수정
    - 동일 레벨에 대한 find의 경우 항상 첫번 째 level만 획득할 수 있는 버그 수정
  
  * DeviceInfo 클래스 구현
    - plist의 특징상 일반 xml처럼 해석할 수 없기 때문에 항상 이런 wrapper 클래스를 만들어
      application 마다 다르게 처리한다.

