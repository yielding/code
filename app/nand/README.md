## TODO
  * source file 구조 개선
    - 현재 develop 및에 모여진 코드가 일부는 common으로 일부는 nand 및으로 이동해야 한다.

  * NAND class 구현
    - refactoring
      > header 및 객체의 위치를 정확하게 조정한다.

    - read_page
      > SpareData 위치 잡기 정확하게 refactoring

    - NANDRemote
      : readPage

    - VFL
    
    - VSVFL

    - YAFTL

## DOING
  * NAND class 구현

    - ByteBuffer 확장
      > set_uint4_le()

## DONE
  * NAND class 구현
    - read_page
      > SpareData 구조체 해석

    - refactoring
      > DeviceInfo 클래스내에서 해석 및 타입 변환을 완료한다.

    - ByteBuffer 확장
      > starts_with 추가

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

