grammar CreateTable;

table_list 
  : (table_def)* 
  ;

table_def 
  : CREATE tmp? TABLE (database_name DOT)? table_name LPARAN column_def (COMMA column_def)* RPARAN
  ;

tmp
  : TEMP
  | TEMPORARY
  ;

column_def
  : column_name (type_name)? (column_constraint)*
  ;

type_name
  : ID
  | LPARAN signed_number RPARAN
  | LPARAN signed_number COMMA signed_number RPARAN
  ;

column_constraint 
  : (CONSTRAINT constraint_name)?
  ;
  
data_type_def 
  : data_type length_constraint? ;

data_type 
  : ID 
  ;

length_constraint 
  : LPARAN NUMBER RPARAN 
  ;

column_name 
  : ID 
  ;
constraint_name
  : ID
  ;

database_name 
  : ID 
  ;

table_name 
  : ID 
  ;

CONSTRAINT
  : C O N S T R A I N T
  ;

CREATE
  : C R E A T E;

TABLE
  : T A B L E;

TEMP
  : T E M P;

TEMPORARY
  : T E M P O R A R Y;

NOT
  : N O T;

NULL
  : N U L L ;

PRIMARY
  : P R I M A R Y;

KEY
  : K E Y;

AUTOINCREMENT
  : A U T O I N C R E M E N T;

DEFAULT
  : D E F A U L T;

////////////////////////////////////////////////////////////////////////////////
//
//
////////////////////////////////////////////////////////////////////////////////
LPARAN    : '(';
RPARAN    : ')';
COMMA     : ',';
SEMICOLON : ';';
DOT       : '.';
MINUS     : '-';
NUMBER    : (MINUS)? (DIGIT)+;
ID        : (('a'..'z'|'A'..'Z' | '_' |'\'') ((DIGIT)*))+ ;
NEWLINE   :'\r'? '\n';
//WS        : ('\t' | ' ' | '\r' | '\n' | '\u000C')+ -> skip ;
WS        : [ \t\r\n]+ -> skip;

fragment DIGIT   : [0-9];

fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
