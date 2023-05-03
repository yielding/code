#!/usr/bin/env php
<?
   $b = array("이창하", "박기원", "박재경", "조창현");
   $a = array("bbb", "cccc", "aaa");
   sort($a);
   sort($b);
   for($i=0; $i<4; $i++) print $a[$i];
   for($i=0; $i<4; $i++) print $b[$i];
?>
