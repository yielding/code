#!/usr/local/php4/bin/php -q
<?
   $DB_HOME= "/home/joonim/public_html/adodb/adodb.inc.php";
   include($DB_HOME);

   ADOLoadCode("mysql");
   $conn  = &ADONewConnection();
   $conn->Connect("localhost", "root","1234", "leech");

   $sql   = "select regdate, memo from memo"; 
   $rs    = $conn->Execute($sql);
   $num   = $rs->RowCount();

   for ($i=0; $i<$num; $i++) {
       $memo = $rs->Fields("memo");
       $date = $rs->Fields("regdate");
       print "$i> ";
       print $memo . "\n";
       print $date . "\n";
       $rs->MoveNext();
   }
?>
