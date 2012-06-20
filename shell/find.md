## find로 cpp, h 에서 원하는 pattern을 찾기
  - find -name \*.h -print -o -name \*.cpp -print | xargs grep -rn pattern

## p5-app-ack로 
  - ack pattern --cpp
  - "--cpp" by default matches .cpp, .cc, .cxx, .m, .hpp, .h, .hxx files 
