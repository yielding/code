## 스크립트 기반 분석기
  * REPL ?

## 분석기 자료구조 분석
### Cellebrite Physical Analyzer
  * Dump (Data Store)
    - Extraction data
      > Device information
      > Image Hash information
      > Device Info (IMSI)
      > Device Context 
        : Phone data
        : Data files

    - Device Info
      > General
      > System
      > SIM
      > Bluetooth

    - Images
      image를 보고 할 수 있는 모든 tool window가 다 열린다.
      > 

    - Memory ranges
      image에서 분석된 영역에 대한 목록
      >

    - File Systems
      파일 시스템에 삭제 파일 까지 다 보여준다.
        > KFAT0
        > SamsungMCU

    - Analyzed data
        > Call Log (3)
        > Contacts (1255)
        > SMS Messages (192)

    - Data files
      파일 시스템 보기와 다른 점은 그루핑 방식이 다르다는 것, 아래처럼
        > Images
        > Videos
        > Audio
        > Text

    - Tags
      특별히 관심있는 데이타 타입에 대해 다시 그룹으로 묶은 node
      grouping 위의 Data files와 같다.

    - Reports

### MDSmart
  * Case
    - name
    - examiner

  * Evidences (Image == memory module)

  * FileBase

  * Chunk
    - Image의 일부 영역

  * Analyzed Data
    - Contacts
    - Calendar
    - Chats
    - SMS
    - Emails
    - CallLog
    - Notes
    - Web Bookmarks
    - Web History
    - Locations
    - Audio
    - Images
    - Video
    - User Dictionary
    - Applications

## 분석기 스크립트 실행기

