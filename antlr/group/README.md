HOWTO
=====
  1. antlr4 Data.g4

  2. javac DataGrouper.java Data\*.java

  3. java DataGrouper <enter>
     2 9 10 3 1 2 3

  4. result
     (file (group 2 (sequence 9 10)) (group 3 (sequence 1 2 3)))
