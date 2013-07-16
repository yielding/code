grammar CreateTable;
 
@header {
import java.util.ArrayList;
import java.util.List;
 
}
 
table_list returns [List tables] 
    : {$tables = new ArrayList();}  (table_def { $tables.add($table_def.tbl); } )*  ;
     
 
table_def returns [Table tbl]
    : 'create' 'table' table_name 
        table_element_list {
            $tbl = new Table(String.valueOf($table_name.text), $table_element_list.cols);
        }
        SEMICOLON ;
 
table_name 
    :   (schema DOT)? table  ;
 
table_element_list returns [List cols]
    : {$cols = new ArrayList();} LEFT_PAREN te=table_element {$cols.add($te.col);} (COMMA te=table_element {$cols.add($te.col);})*   RIGHT_PAREN ;
 
table_element returns [Column col]
    :  column_name data_type_def (column_constraint)? {$col = new Column($column_name.cn, $data_type_def.tpe, $column_constraint.text, $data_type_def.len);};
 
column_name returns [String cn]
    :   ID {$cn = String.valueOf($ID.text); };
 
column_constraint
    :   'not' 'null';
 
data_type_def returns [String tpe, Integer len]
    : data_type {$tpe = $data_type.tp;} length_constraint? {$len = $length_constraint.len;}      
    ;
     
data_type returns [String tp]
    : 
    ((('character' 'varying')  | 'varchar') { $tp = "varchar"; }
        | ('bit' 'varying' | 'varbit')      { $tp = "varbit";  }
        | ('double' 'precision' | 'float8') { $tp = "float8";  }
        | ('character' | 'char')            { $tp = "char";    }  
        | ('integer' | 'int' | 'int4')      { $tp = "integer"; }
        | ID                                { $tp = $ID.text;  }
        );
     
 
length_constraint returns [Integer len] 
    : LEFT_PAREN NUMBER RIGHT_PAREN {$len = Integer.valueOf($NUMBER.text);}     ;
     
schema : ID;
table : ID;
 
/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/
 
LEFT_PAREN : '(';
RIGHT_PAREN : ')';
COMMA : ',';
SEMICOLON : ';';
DOT :    '.';
NUMBER  :   (DIGIT)+;   
ID  : (('a'..'z'|'A'..'Z' | '_') ((DIGIT)*))+ ;
NEWLINE:'\r'? '\n' ;
WS : ( '\t' | ' ' | '\r' | '\n' | '\u000C' )+   { $channel = HIDDEN; } ;
fragment DIGIT :   '0'..'9' ;