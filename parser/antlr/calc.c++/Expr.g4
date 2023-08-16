grammar Expr;

prog
  : plusOrMinus NL? EOF
  ;

plusOrMinus
  : plusOrMinus PLUS  multOrDiv # PlusOp
  | plusOrMinus MINUS multOrDiv # BinaryMinusOp
  | multOrDiv                   # ToMultOrDiv
  ;

multOrDiv
  : multOrDiv MULT atom         # MultOp
  | multOrDiv DIV  atom         # DivOp
  | atom                        # toAtom
  ;

atom
  : INT                         # Int
  | MINUS plusOrMinus           # UnaryMinusOp
  | POPEN plusOrMinus PCLOSE    # ParenthesisOp
  ;

INT     : [0-9]+ ;
NL      : '\r'? '\n' ;
WS      : [ \t]+ -> skip ;
PLUS    : '+';
MINUS   : '-';
MULT    : '*';
DIV     : '/';
POPEN   : '(';
PCLOSE  : ')';