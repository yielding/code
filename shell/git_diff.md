Git Commands
============

1. diff

  * git diff | mate

  * git diff | vim -R -
    -R : read only
    -  : standard input

  * 특정 파일의 변경내역 보기
    git diff bd1234 bc1313 my-file

  * 변경 파일 목록 구하기
    git diff --name-only foo bar (foo, bar: revision)

  * state된 파일과 HEAD
    git diff --staged/--cached

2. log
  * 변경 파일 날짜와 revision no 구하기
    git log | egrep "commit|Date"
