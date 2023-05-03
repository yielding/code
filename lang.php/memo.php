#!/usr/local/php4/bin/php -q
<?
   $db  = mysql_connect("localhost", "root", "1234");
   mysql_select_db("leech", $db);

   $sql = "select regdate, memo from memo"; 
   $res = mysql_query($sql, $db);
   $num = mysql_num_rows($res);

   for ($i=0; $i<$num; $i++) 
   {
       $memo = mysql_result($res, $i, "memo");
       $date = mysql_result($res, $i, "regdate");
       print "$i> ";
       print $memo . "\n";
       print $date . "\n";
   }
?>
