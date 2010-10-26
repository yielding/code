#!/usr/bin/php -q
<?

function getRDate(&$arr, $day, $count) {
    for ($i=0; $i<$count; $i++) {
        $arr[$i] = sprintf("%02d", rand(1, $day));
    }
    sort($arr);
}

srand((double)microtime() * 1000000);

$index = array();
$day   = 31;
$count = 10;
getRDate($index, $day, $count);

$db = mysql_connect("www.tinc.co.kr", "webmaster", "web1234");
mysql_select_db("tincdb001", $db);

$sql = "select * from home_user order by passwd desc";
$res = mysql_query($sql, $db);
$num = mysql_num_rows($res);

for ($i=0; $i<$count; $i++) {
    $name  = mysql_result($res, $i, "name");
    $svcid = mysql_result($res, $i, "id");
    $sql   = "update sys_svc_users 
    set regdt   = to_date('2000/80/$index[$i]', 'yyyy/mm/dd')
    where svcid = '$svcid'";
    echo "$sql\n";
}

?>
