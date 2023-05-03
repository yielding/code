#!/usr/bin/php -q
<?
   $string = "leech123456";

   if (ereg("(123)(456)", $string, $regs))
       echo $regs[1], $regs[2];
?>
