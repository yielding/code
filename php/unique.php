#!/usr/bin/php -q
<?
    function unique($a) {
        sort($a);
        for ($i=0; $i<sizeof($a) - 1; $i++) {
            if (strcmp($a[$i], $a[$i+1]) == 0) return 0;
        }
        return 1;
    }

    $a = array( "aaa", "aaa", "zzz", "bbb", "ddd"); 
    echo unique($a);
?>
