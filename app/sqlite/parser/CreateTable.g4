grammar CreateTable;

tableList
  : (createTableStmt)* 
  ;

createTableStmt
  : CREATE tmp? TABLE (IF NOT EXISTS)? (databaseName POINT)? tableName LP columnDefs (tableConstraints)? RP SEMICOLON
  ;

tmp
  : TEMP 
  | TEMPORARY
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
  : (CONSTRAINT name)? PRIMARY KEY (ASC|DESC)? conflictClause AUTOINCREMENT 
  | (CONSTRAINT name)? NOT NULL conflictClause
  | (CONSTRAINT name)? UNIQUE conflictClause
  | (CONSTRAINT name)? DEFAULT (SIGNED_NUMBER|LITERAL_VALUE)
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
  : (CONSTRAINT name)? (PRIMARY KEY|UNIQUE) LP indexedColumn (COMMA indexedColumn)* RP conflictClause
  | (CONSTRAINT name)? FOREIGN KEY LP columnNames RP foreignKeyCaluse
  ;

conflictClause
  : (ON CONFLICT)?
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
  : ID
  ;

ABORT         : A B O R T;
ASC           : A S C;
AUTOINCREMENT : A U T O I N C R E M E N T ;
CHECK         : C H E C K ;
CONFLICT      : C O N F L I C T ;
CONSTRAINT    : C O N S T R A I N T ;
COLLATE       : C O L L A T E;
CREATE        : C R E A T E ;

CURRENT_TIME  : C U R R E N T UNDER_BAR T I M E;
CURRENT_DATE  : C U R R E N T UNDER_BAR D A T E;
CURRENT_TIMESTAMP : C U R R E N T UNDER_BAR T I M E S T A M P;
DEFAULT       : D E F A U L T;
DESC          : D E S C;
EXISTS        : E X I S T S;
FAIL          : F A I L;
FOREIGN       : F O R E I G N;
IF            : I F;
IGNORE        : I G N O R E;
KEY           : K E Y ;
NOT           : N O T ;
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


UNDER_BAR       : '_';
POINT           : '.' ;
LP              : '(' ;
RP              : ')' ;
COMMA           : ',' ;
SEMICOLON       : ';' ;
PLUS            : '+' ;
MINUS           : '-' ;
NUMERIC_LITERAL : DIGIT+ (POINT DIGIT*)? ('E' (PLUS|MINUS)? DIGIT+)?  | POINT DIGIT+ ('E' (PLUS|MINUS)? DIGIT+)?  ;
STRING_LITERAL  : '\'' (~'\'')* '\'' ;
BLOB_LITERAL    : 'X' STRING_LITERAL ;
LITERAL_VALUE   : NUMERIC_LITERAL | STRING_LITERAL | BLOB_LITERAL | NULL | CURRENT_TIME | CURRENT_DATE | CURRENT_TIMESTAMP ;
SIGNED_NUMBER   : (PLUS|MINUS)? NUMERIC_LITERAL ;

ID              : LETTER (LETTER|DIGIT)*;
WS              : [ \t\r\n]+ -> skip;
NL              : '\r'? '\n' ;

fragment DIGIT     : [0-9] ;
fragment LETTER    : [a-zA-Z_];

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
