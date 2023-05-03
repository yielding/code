#!/usr/bin/php -q

<?
  function read_file($filename) {
      $fd = fopen($filename, "r");

      $buffer ="";
      while (!feof($fd)) $buffer .= fgets($fd, 1024); 
                             
      fclose($fd);
      return $buffer;
  }

  $contents = read_file("./excel.csv");

  print($contents);


?>
