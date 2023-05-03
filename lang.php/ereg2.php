#!/usr/local/php4/bin/php -q
<?
   $string = "leejch123456";
   if (ereg("[^0-9a-z]", $string))
      echo "박기원 바보";
   else 
      echo "이창하 바보";
?>
