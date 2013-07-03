TO BUILD
========
  antlr4 -no-listener Rows.g4
  javac Rows\*.java Col.java
  java Col 1 < t.rows
