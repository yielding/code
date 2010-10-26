#!/usr/local/php4/bin/php -q
<?
   $db = mysql_connect("localhost", "root", "1234");
   mysql_select_db("joonim_talk2", $db);
   $no = 382;

   $sql = "select text from greeting where num=$no";
   echo $sql;

   $res = mysql_query($sql, $db);
   $num = mysql_num_rows($res);

   for ($i=0; $i<$num; $i++) {
       $text = mysql_result($res, $i, "text");
       print $text . "\n";
   }
?>
