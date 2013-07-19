grammar CreateTable;

tableList
  : (createTableStmt)* 
  ;

createTableStmt
  : CREATE tmp? TABLE (IF 'NOT' EXISTS)? (databaseName POINT)? tableName LP columnDefs (COMMA tableConstraints)? RP SEMICOLON
  ;

tmp
  : TEMP | TEMPORARY
  ;

columnDefs
  : columnDef (COMMA columnDef)*
  ;

columnDef
  : columnName typeName? columnConstraint*
  ;

typeName
  : ID+ LP SIGNED_NUMBER COMMA SIGNED_NUMBER RP
  | ID+ LP SIGNED_NUMBER RP
  | ID+ 
  ;

// REMARK Simplified
columnConstraint
  : (CONSTRAINT name)? PRIMARY KEY (ASC|DESC)? conflictClause AUTOINCREMENT?
  | (CONSTRAINT name)? 'NOT' NULL conflictClause
  | (CONSTRAINT name)? NULL conflictClause
  | (CONSTRAINT name)? UNIQUE conflictClause
  | (CONSTRAINT name)? DEFAULT (SIGNED_NUMBER | literalValue)
  | (CONSTRAINT name)? COLLATE collationName
  | (CONSTRAINT name)? foreignKeyCaluse
  ;

foreignKeyCaluse
  : REFERENCES tableName (LP columnNames RP)?
  ;

tableConstraints
  : tableConstraint (COMMA tableConstraint)*
  ;

// REMARK Simplified
tableConstraint
  : (CONSTRAINT name)? (PRIMARY KEY | UNIQUE) LP indexedColumns RP conflictClause
  | (CONSTRAINT name)? FOREIGN KEY LP columnNames RP foreignKeyCaluse
  ;

conflictClause
  : ON 'CONFLICT' (ROLLBACK | ABORT | FAIL | IGNORE | REPLACE)
  |
  ;

columnNames
  : columnName (COMMA columnName)*
  ;

indexedColumns
  : indexedColumn (COMMA indexedColumn)*
  ;

indexedColumn
  : columnName (COLLATE collationName)? (ASC|DESC)?
  ;

collationName
  : name
  ;

columnName
  : name
  ;

databaseName
  : name
  ;

tableName
  : name
  ;

name
  : STRING_LITERAL | '"' ID '"' | ID
  ;

// using name instead of STRING_LITERAL
literalValue
  : NUMERIC_LITERAL 
  | BLOB_LITERAL 
  | name 
  | NULL
  | 'CURRENT_TIME'
  | 'CURRENT_DATE '
  | 'CURRENT_TIMESTAMP'
  ;

NUMERIC_LITERAL 
  : DIGIT+ (POINT DIGIT*)? ('E' (PLUS|MINUS)? DIGIT+)? 
  | POINT DIGIT+ ('E' (PLUS|MINUS)? DIGIT+)?
  ;

SIGNED_NUMBER
  : (PLUS|MINUS)? NUMERIC_LITERAL
  ;

BLOB_LITERAL   : ('x'|'X') STRING_LITERAL ;
STRING_LITERAL : '\'' (~'\'')* '\'' ;

POINT         : '.' ;
LP            : '(' ;
RP            : ')' ;
COMMA         : ',' ;
SEMICOLON     : ';' ;
PLUS          : '+' ;
MINUS         : '-' ;

ABORT         : A B O R T;
ASC           : A S C;
AUTOINCREMENT : A U T O I N C R E M E N T ;
// CHECK         : C H E C K ;
// CONFLICT      : C O N F L I C T ;
CONSTRAINT    : C O N S T R A I N T ;
COLLATE       : C O L L A T E;
CREATE        : C R E A T E ;
DEFAULT       : D E F A U L T;
DESC          : D E S C;
EXISTS        : E X I S T S;
FAIL          : F A I L;
FOREIGN       : F O R E I G N;
IF            : I F;
IGNORE        : I G N O R E;
KEY           : K E Y ;
// NOT           : N O T ;
NULL          : N U L L ;
ON            : O N;
PRIMARY       : P R I M A R Y ;
REFERENCES    : R E F E R E N C E S;
REPLACE       : R E P L A C E;
ROLLBACK      : R O L L B A C K;
TABLE         : T A B L E ;
TEMP          : T E M P ;
TEMPORARY     : T E M P O R A R Y ;
UNIQUE        : U N I Q U E ;

WS              : [ \t\r\n\f]+ -> channel(HIDDEN);
ID              : LETTER (LETTER|DIGIT)*;
fragment DIGIT  : [0-9] ;
fragment LETTER : [a-zA-Z_];
NL              : '\r'? '\n' ;

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
