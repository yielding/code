1. antlr4 -no-listener -visitor LabeledExpr.g4
2. javac Calc.java LabeledExpr*.java
3. java Calc t.expr
