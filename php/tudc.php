#!/usr/bin/env php -q
<?
// 컬럼에 있는 널을 제외한 데이타와 개수를 리턴
function realData($res, $col, &$arr) 
{ 
    $num   = mysql_num_rows($res);
    $count = 0;

    for ($i=0; $i<$num; $i++) {
        $x = mysql_result($res, $i, $col);
        if (trim($x) == "") continue;
        $arr[$count] = $x;
        $count++;
    }
    return $count;
} 

$db = mysql_connect("www.tinc.co.kr", "webmaster", "web1234");
mysql_select_db("tudc_db", $db);

$sql   = "select * from TUDC_SULMUN1";
$res   = mysql_query($sql, $db);

$name  = "Q06";
$a     = array();
$count = realData($res, $name, $a);

for ($i=0; $i<$count; $i++) echo "$a[$i], ";

?>
