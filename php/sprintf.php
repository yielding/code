#!/usr/bin/env php -q

<?
   for ($i=0; $i<20; $i++) {
       $buf = sprintf("%05d", $i);
       echo $buf . "\n";
   }
?>
