grammar CreateTable;

table_list 
  : (table_def)* ;

table_def 
  : 'CREATE' 'TABLE' table_name table_element_list ';' ;

table_name
  : (schema DOT)? table ;

table_element_list 
  : LPARAN table_element (COMMA table_element)* RPARAN ;

table_element 
  : column_name data_type_def column_constraint? ;

column_name 
  : ID ;

column_constraint 
  : 'NOT' 'NULL' ;
  
data_type_def 
  : data_type length_constaint? ;

data_type 
  : (('character' 'varying')  | 'varchar')
  | ('character' | 'char')
  | ID ;

length_constaint 
  : LPARAN NUMBER RPARAN ;

schema 
  : ID ;

table 
  : ID ;

/*-------------------------------------------------------------
 *
 *-------------------------------------------------------------*/

LPARAN    : '(';
RPARAN    : ')';
COMMA     : ',';
DOT       : '.';
NUMBER    : (DIGIT)+;
ID        : (('a'..'z'|'A'..'Z' | '_') ((DIGIT)*))+ ;
NEWLINE   :'\r'? '\n' ;
WS        : ( '\t' | ' ' | '\r' | '\n' | '\u000C' )+ -> skip ;

fragment 
DIGIT     : [0-9];
